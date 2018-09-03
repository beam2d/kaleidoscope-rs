extern crate llvm_sys;

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::str::Chars;
use std::io::{self, Read, Write};
use std::iter::Peekable;

use llvm_sys::prelude::*;

fn cstr(s: &str) -> Box<CStr> {
    CString::new(s).unwrap().into_boxed_c_str()
}

type ParseResult<T> = Result<T, Cow<'static, str>>;
type CompileResult<T> = Result<T, Cow<'static, str>>;

#[derive(Debug, PartialEq)]
enum Token {
    Eof,

    Def,
    Extern,

    Ident(String),
    Number(f64),

    Op(char),
}

impl Token {
    fn get_op_name_or_panic(&self) -> char {
        match *self {
            Token::Op(op) => op,
            _ => panic!("op is required"),
        }
    }
}

struct Lexer<'a> {
    src: Peekable<Chars<'a>>,
    next: Option<Token>,
}

impl<'a> Lexer<'a> {
    fn new(src_text: &'a str) -> Lexer<'a> {
        Lexer { src: src_text.chars().peekable(), next: None }
    }

    fn gettok(&mut self) -> ParseResult<Token> {
        if let Some(tok) = self.next.take() {
            return Ok(tok);
        }

        // skip any whitespace
        loop {
            match self.peekc() {
                Some(&'#') => {  // comment
                    while let Some(c) = self.getc() {
                        if c == '\r' || c == '\n' {
                            break;
                        }
                    }
                },
                Some(&c) if c.is_whitespace() => { self.getc(); },
                Some(_) => break,
                None => return Ok(Token::Eof),
            }
        }

        let c = *self.peekc().unwrap();

        // identifier: [a-zA-Z][a-zA-Z0-9]*
        if c.is_ascii_alphabetic() {
            let ident = self.read_while(|c| c.is_ascii_alphanumeric());
            return match &ident[..] {
                "def" => Ok(Token::Def),
                "extern" => Ok(Token::Extern),
                _ => Ok(Token::Ident(ident)),
            }
        }

        // number: [0-9.]+
        let is_numeric = |c: char| c.is_ascii_digit() || c == '.';
        if is_numeric(c) {
            let num_str = self.read_while(is_numeric);
            let num = num_str.parse().map_err(|_| Cow::from("cannot parse number"))?;
            return Ok(Token::Number(num));
        }

        // operator
        match self.getc() {
            Some(c) => Ok(Token::Op(c)),
            None => Ok(Token::Eof),
        }
    }

    fn peektok(&mut self) -> ParseResult<&Token> {
        match self.next {
            Some(ref tok) => Ok(tok),
            None => {
                self.next = Some(self.gettok()?);
                Ok(self.next.as_ref().unwrap())
            }
        }
    }

    // Reads from src while pred returns true.
    // It assumes that pred holds for the first character (thus skipping test with pred).
    fn read_while(&mut self, pred: impl Fn(char) -> bool) -> String {
        let mut s = self.getc().unwrap().to_string();
        while let Some(&c) = self.peekc() {
            if !pred(c) {
                break;
            }
            s.push(self.getc().unwrap());
        }
        s
    }

    fn peekc(&mut self) -> Option<&char> {
        self.src.peek()
    }

    fn getc(&mut self) -> Option<char> {
        self.src.next()
    }
}

struct LLVM {
    ctx: LLVMContextRef,
    builder: LLVMBuilderRef,
    module: LLVMModuleRef,
    named_values: HashMap<String, LLVMValueRef>,
}

impl LLVM {
    fn new() -> LLVM {
        let name = cstr("my cool jit");
        unsafe {
            let ctx = llvm_sys::core::LLVMContextCreate();
            let builder = llvm_sys::core::LLVMCreateBuilder();
            let module = llvm_sys::core::LLVMModuleCreateWithNameInContext(name.as_ptr(), ctx);
            LLVM { ctx, builder, module, named_values: HashMap::new() }
        }
    }
}

#[derive(Debug)]
enum ExprAst {
    Number(f64),
    Variable(String),
    Binary { op: char, lhs: Box<ExprAst>, rhs: Box<ExprAst> },
    Call { callee: String, args: Vec<ExprAst> },
}

impl ExprAst {
    fn codegen(&self, llvm: &mut LLVM) -> CompileResult<LLVMValueRef> {
        match self {
            ExprAst::Number(value) => unsafe {
                let ty = llvm_sys::core::LLVMDoubleTypeInContext(llvm.ctx);
                Ok(llvm_sys::core::LLVMConstReal(ty, *value))
            },
            ExprAst::Variable(name) => {
                match llvm.named_values.get(name) {
                    Some(value) => Ok(*value),
                    None => Err(Cow::from("unknown variable name")),
                }
            },
            ExprAst::Binary { op, lhs, rhs } => {
                let lhs = lhs.codegen(llvm)?;
                let rhs = rhs.codegen(llvm)?;
                match op {
                    '+' => unsafe {
                        let name = cstr("addtmp");
                        Ok(llvm_sys::core::LLVMBuildFAdd(llvm.builder, lhs, rhs, name.as_ptr()))
                    },
                    '-' => unsafe {
                        let name = cstr("subtmp");
                        Ok(llvm_sys::core::LLVMBuildFSub(llvm.builder, lhs, rhs, name.as_ptr()))
                    },
                    '*' => unsafe {
                        let name = cstr("multmp");
                        Ok(llvm_sys::core::LLVMBuildFMul(llvm.builder, lhs, rhs, name.as_ptr()))
                    },
                    '<' => unsafe {
                        let name_cmp = cstr("cmptmp");
                        let name_bool = cstr("booltmp");

                        let b = llvm_sys::core::LLVMBuildFCmp(
                            llvm.builder, llvm_sys::LLVMRealPredicate::LLVMRealULT, lhs, rhs, name_cmp.as_ptr());
                        let ty = llvm_sys::core::LLVMDoubleTypeInContext(llvm.ctx);
                        Ok(llvm_sys::core::LLVMBuildUIToFP(llvm.builder, b, ty, name_bool.as_ptr()))
                    },
                    _ => Err(Cow::from("invalid binary operator")),
                }
            },
            ExprAst::Call { callee, args } => {
                let name = cstr(&callee);
                let callee_f = unsafe { llvm_sys::core::LLVMGetNamedFunction(llvm.module, name.as_ptr()) };

                let mut args_v = vec![];
                for arg in args {
                    let arg_v = arg.codegen(llvm)?;
                    if arg_v.is_null() {
                        return Err(Cow::from("invalid function argument"));
                    }
                    args_v.push(arg_v);
                }
                let n_args = unsafe { llvm_sys::core::LLVMCountParams(callee_f) };
                if n_args as usize != args_v.len() {
                    return Err(Cow::from(format!("invalid number of arguments: {} vs {}", n_args, args_v.len())));
                }
                let name = cstr("calltmp");
                unsafe {
                    Ok(llvm_sys::core::LLVMBuildCall(llvm.builder, callee_f, args_v.as_mut_ptr(), n_args, name.as_ptr()))
                }
            },
        }
    }
}

#[derive(Debug)]
struct PrototypeAst {
    name: String,
    args: Vec<String>,
}

impl PrototypeAst {
    fn new(name: String, args: Vec<String>) -> PrototypeAst {
        PrototypeAst { name, args }
    }

    fn codegen(&self, llvm: &mut LLVM) -> LLVMValueRef {
        let n_args = self.args.len();
        let double = unsafe { llvm_sys::core::LLVMDoubleTypeInContext(llvm.ctx) };
        let mut doubles = vec![double; n_args];
        let ft = unsafe { llvm_sys::core::LLVMFunctionType(double, doubles.as_mut_ptr(), n_args as u32, 0) };
        let name = cstr(&self.name);

        // TODO(beam2d): linkage?
        let f = unsafe { llvm_sys::core::LLVMAddFunction(llvm.module, name.as_ptr(), ft) };

        for i in 0..n_args {
            let param = unsafe { llvm_sys::core::LLVMGetParam(f, i as u32) };
            let name = cstr(&self.args[i]);
            unsafe { llvm_sys::core::LLVMSetValueName(param, name.as_ptr()) };
        }
        f
    }
}

#[derive(Debug)]
struct FunctionAst {
    proto: PrototypeAst,
    body: ExprAst,
}

impl FunctionAst {
    fn new(proto: PrototypeAst, body: ExprAst) -> FunctionAst {
        FunctionAst { proto, body }
    }

    fn codegen(&self, llvm: &mut LLVM) -> CompileResult<LLVMValueRef> {
        let name = cstr(&self.proto.name);
        let mut f = unsafe { llvm_sys::core::LLVMGetNamedFunction(llvm.module, name.as_ptr()) };
        if f.is_null() {
            f = self.proto.codegen(llvm);
            if f.is_null() {
                return Err(Cow::from("function cannot be created"));
            }
        }
        let empty = unsafe { llvm_sys::core::LLVMCountBasicBlocks(f) } == 0;
        if !empty {
            return Err(Cow::from("function cannot be redefined"));
        }

        let name = cstr("entry");
        unsafe {
            let bb = llvm_sys::core::LLVMAppendBasicBlock(f, name.as_ptr());
            llvm_sys::core::LLVMPositionBuilderAtEnd(llvm.builder, bb);
        }

        llvm.named_values.clear();
        let n_args = self.proto.args.len();
        for i in 0..n_args {
            let param = unsafe { llvm_sys::core::LLVMGetParam(f, i as u32) };
            llvm.named_values.insert(self.proto.args[i].clone(), param);
        }

        match self.body.codegen(llvm) {
            Ok(ret_val) => unsafe {
                llvm_sys::core::LLVMBuildRet(llvm.builder, ret_val);
                match llvm_sys::analysis::LLVMVerifyFunction(f, llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction) {
                    0 => Ok(f),
                    _ => {
                        llvm_sys::core::LLVMDeleteFunction(f);
                        Err(Cow::from("invalid function"))
                    },
                }
            },
            Err(e) => unsafe {
                llvm_sys::core::LLVMDeleteFunction(f);
                Err(e)
            },
        }
    }
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    binop_precedence: HashMap<char, i32>,
}

impl<'a> Parser<'a> {
    fn new(src_text: &'a str) -> Parser<'a> {
        let mut binop_precedence = HashMap::new();
        binop_precedence.insert('<', 10);
        binop_precedence.insert('+', 20);
        binop_precedence.insert('-', 20);
        binop_precedence.insert('*', 40);
        Parser { lexer: Lexer::new(src_text), binop_precedence }
    }

    // ident_expr
    //     ::= ident
    //     ::= ident '(' expr* ')'
    fn parse_ident_expr(&mut self, name: String) -> ParseResult<ExprAst> {
        match self.lexer.peektok()? {
            Token::Op('(') => { self.lexer.gettok().unwrap(); },
            _ => return Ok(ExprAst::Variable(name)),
        }

        let mut args = vec![];
        while *self.lexer.peektok()? != Token::Op(')') {
            let arg = self.parse_expr()?;
            args.push(arg);
            if let Token::Op(')') = self.lexer.peektok()? {
                break;
            }
            self.consume_tok(&Token::Op(','))?;
        }
        self.lexer.gettok().unwrap();  // Op(')')

        Ok(ExprAst::Call { callee: name, args })
    }

    // primary_expr
    //     ::= ident_expr
    //     ::= num_expr
    //     ::= parsen_expr
    fn parse_primary_expr(&mut self) -> ParseResult<ExprAst> {
        match self.lexer.gettok()? {
            Token::Ident(name) => self.parse_ident_expr(name),
            Token::Number(value) => Ok(ExprAst::Number(value)),
            Token::Op('(') => {
                let expr = self.parse_expr()?;
                self.consume_tok(&Token::Op(')'))?;
                Ok(expr)
            },
            _ => Err(Cow::from("unknown token when expecting an expression")),
        }
    }

    // binop_rhs ::= ('+' primary)+
    fn parse_binop_rhs(&mut self, prec: i32, mut lhs: ExprAst) -> ParseResult<ExprAst> {
        loop {
            let tok_prec = self.get_prec()?;
            if tok_prec < prec {
                return Ok(lhs);
            }

            let op = self.lexer.gettok()?.get_op_name_or_panic();
            let mut rhs = self.parse_primary_expr()?;

            let next_prec = self.get_prec()?;
            if prec < next_prec {
                rhs = self.parse_binop_rhs(tok_prec + 1, rhs)?;
            }

            lhs = ExprAst::Binary { op, lhs: Box::from(lhs), rhs: Box::from(rhs) };
        }
    }

    // expr ::= primary binop_rhs
    fn parse_expr(&mut self) -> ParseResult<ExprAst> {
        let lhs = self.parse_primary_expr()?;
        self.parse_binop_rhs(0, lhs)
    }

    // prototype ::= ident '(' ident* ')'
    fn parse_prototype(&mut self) -> ParseResult<PrototypeAst> {
        let name = self.consume_ident_tok()?;
        self.consume_tok(&Token::Op('('))?;

        let mut args = vec![];
        while let Token::Ident(_) = self.lexer.peektok()? {
            args.push(self.consume_ident_tok()?);
        }
        self.consume_tok(&Token::Op(')'))?;

        Ok(PrototypeAst::new(name, args))
    }

    // definition ::= 'def' prototype expr
    fn parse_definition(&mut self) -> ParseResult<FunctionAst> {
        self.consume_tok(&Token::Def)?;
        let proto = self.parse_prototype()?;
        let body = self.parse_expr()?;
        Ok(FunctionAst::new(proto, body))
    }

    // external ::= 'extern' prototype
    fn parse_extern(&mut self) -> ParseResult<PrototypeAst> {
        self.consume_tok(&Token::Extern)?;
        Ok(self.parse_prototype()?)
    }

    // toplevelexpr ::= expr
    fn parse_toplevel_expr(&mut self) -> ParseResult<FunctionAst> {
        let expr = self.parse_expr()?;
        let proto = PrototypeAst::new("".to_string(), vec![]);
        Ok(FunctionAst::new(proto, expr))
    }

    // ---- helper functions ----

    // Consumes the specified token
    fn consume_tok(&mut self, tok: &Token) -> ParseResult<()> {
        if self.lexer.gettok()? != *tok {
            Err(Cow::from(format!("{:?} is expected", tok)))?;
        }
        Ok(())
    }

    // Consumes an identifier
    fn consume_ident_tok(&mut self) -> ParseResult<String> {
        match self.lexer.gettok()? {
            Token::Ident(n) => Ok(n),
            _ => Err(Cow::from("identifier is expected")),
        }
    }

    // Gets the precedence of the next binop token or -1 if the next is not a binop
    fn get_prec(&mut self) -> ParseResult<i32> {
        if let Token::Op(op) = self.lexer.peektok()? {
            return Ok(*self.binop_precedence.get(op).unwrap_or(&-1));
        }
        Ok(-1)
    }
}

fn write_err(s: &str) {
    write!(io::stderr(), "{}", s).unwrap();
}

fn handle_def(parser: &mut Parser, llvm: &mut LLVM) -> ParseResult<()> {
    let fn_ast = parser.parse_definition()?;
    let fn_ir = fn_ast.codegen(llvm)?;
    if !fn_ir.is_null() {
        write_err("Read function definition:");
        unsafe {
            llvm_sys::core::LLVMDumpValue(fn_ir);
        }
        write_err("\n");
    }
    Ok(())
}

fn handle_extern(parser: &mut Parser, llvm: &mut LLVM) -> ParseResult<()> {
    let proto_ast = parser.parse_extern()?;
    let fn_ir = proto_ast.codegen(llvm);
    if !fn_ir.is_null() {
        write_err("Read extern:");
        unsafe {
            llvm_sys::core::LLVMDumpValue(fn_ir);
        }
        write_err("\n");
    }
    Ok(())
}

fn handle_toplevel_expr(parser: &mut Parser, llvm: &mut LLVM) -> ParseResult<()> {
    let fn_ast = parser.parse_toplevel_expr()?;
    let fn_ir = fn_ast.codegen(llvm)?;
    if !fn_ir.is_null() {
        write_err("Read top-level expression:");
        unsafe {
            llvm_sys::core::LLVMDumpValue(fn_ir);
        }
        write_err("\n");
    }
    println!("{:?}", parser.parse_toplevel_expr()?);
    Ok(())
}

fn main() -> Result<(), Cow<'static, str>> {
    let stdin = io::stdin();
    let mut src = String::new();
    stdin.lock().read_to_string(&mut src).map_err(|_| Cow::from("cannot read stdin"))?;

    let mut parser = Parser::new(&src);
    let mut llvm = LLVM::new();
    loop {
        match parser.lexer.peektok()? {
            Token::Eof => break,
            Token::Op(';') => { parser.lexer.gettok()?; },
            Token::Def => handle_def(&mut parser, &mut llvm)?,
            Token::Extern => handle_extern(&mut parser, &mut llvm)?,
            _ => handle_toplevel_expr(&mut parser, &mut llvm)?,
        }
    }

    Ok(())
}
