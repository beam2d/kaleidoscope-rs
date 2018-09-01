use std::borrow::Cow;
use std::str::Chars;
use std::io::{self, Read};
use std::iter::Peekable;

#[derive(Debug)]
enum Token {
    Eof,

    Def,
    Extern,

    Ident(String),
    Number(f64),

    Op(char),
}

impl Token {
    fn eof_as_none(self) -> Option<Token> {
        match self {
            Token::Eof => None,
            tok => Some(tok),
        }
    }
}

struct Lexer<'a> {
    src: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(src_text: &'a str) -> Lexer<'a> {
        Lexer { src: src_text.chars().peekable() }
    }

    fn gettok(&mut self) -> Result<Token, Cow<'static, str>> {
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

fn main() -> Result<(), Cow<'static, str>> {
    let stdin = io::stdin();
    let mut src = String::new();
    stdin.lock().read_to_string(&mut src).map_err(|_| Cow::from("cannot read stdin"))?;
    let mut lexer = Lexer::new(&src);

    while let Some(tok) = lexer.gettok()?.eof_as_none() {
        println!("{:?}", tok);
    }

    Ok(())
}
