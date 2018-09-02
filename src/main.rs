use std::borrow::Cow;
use std::collections::HashMap;
use std::str::Chars;
use std::io::{self, Read};
use std::iter::Peekable;

type ParseResult<T> = Result<T, Cow<'static, str>>;

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
            match self.src.peek() {
                Some(&'#') => {  // comment
                    while let Some(c) = self.src.next() {
                        if c == '\r' || c == '\n' {
                            break;
                        }
                    }
                },
                Some(&c) if c.is_whitespace() => { self.src.next(); },
                Some(_) => break,
                None => return Ok(Token::Eof),
            }
        }

        let c = *self.src.peek().unwrap();

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
        match self.src.next() {
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
        let mut s = self.src.next().unwrap().to_string();
        while let Some(&c) = self.src.peek() {
            if !pred(c) {
                break;
            }
            s.push(self.src.next().unwrap());
        }
        s
    }
}

#[derive(Debug)]
struct BinaryExprAst {
    op: char,
    lhs: Box<ExprAst>,
    rhs: Box<ExprAst>,
}

impl BinaryExprAst {
    fn new(op: char, lhs: ExprAst, rhs: ExprAst) -> BinaryExprAst {
        BinaryExprAst { op, lhs: Box::new(lhs), rhs: Box::new(rhs) }
    }
}

#[derive(Debug)]
struct CallExprAst {
    callee: String,
    args: Vec<ExprAst>,
}

impl CallExprAst {
    fn new(callee: String, args: Vec<ExprAst>) -> CallExprAst {
        CallExprAst { callee, args }
    }
}

#[derive(Debug)]
enum ExprAst {
    Number(f64),
    Variable(String),
    Binary(BinaryExprAst),
    Call(CallExprAst),
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
    //     ::= ident '(' expr ')'
    fn parse_ident_expr(&mut self, name: String) -> ParseResult<ExprAst> {
        match self.lexer.peektok()? {
            Token::Op('(') => { self.lexer.gettok().unwrap(); },
            _ => return Ok(ExprAst::Variable(name)),
        }

        let mut args = vec![];
        while *self.lexer.peektok()? != Token::Op('(') {
            let arg = self.parse_expr()?;
            if let Token::Op(')') = self.lexer.peektok()? {
                break;
            }
            self.consume_tok(&Token::Op(','))?;
            args.push(arg);
        }
        self.lexer.gettok().unwrap();  // Op(')')

        Ok(ExprAst::Call(CallExprAst::new(name, args)))
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

            let bin_op = self.lexer.gettok()?.get_op_name_or_panic();
            let mut rhs = self.parse_primary_expr()?;

            let next_prec = self.get_prec()?;
            if prec < next_prec {
                rhs = self.parse_binop_rhs(tok_prec + 1, rhs)?;
            }

            lhs = ExprAst::Binary(BinaryExprAst::new(bin_op, lhs, rhs));
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

fn handle_def(parser: &mut Parser) -> ParseResult<()> {
    println!("{:?}", parser.parse_definition()?);
    Ok(())
}

fn handle_extern(parser: &mut Parser) -> ParseResult<()> {
    println!("{:?}", parser.parse_extern()?);
    Ok(())
}

fn handle_toplevel_expr(parser: &mut Parser) -> ParseResult<()> {
    println!("{:?}", parser.parse_toplevel_expr()?);
    Ok(())
}

fn main() -> Result<(), Cow<'static, str>> {
    let stdin = io::stdin();
    let mut src = String::new();
    stdin.lock().read_to_string(&mut src).map_err(|_| Cow::from("cannot read stdin"))?;

    let mut parser = Parser::new(&src);
    loop {
        match parser.lexer.peektok()? {
            Token::Eof => break,
            Token::Op(';') => { parser.lexer.gettok()?; },
            Token::Def => handle_def(&mut parser)?,
            Token::Extern => handle_extern(&mut parser)?,
            _ => handle_toplevel_expr(&mut parser)?,
        }
    }

    Ok(())
}
