use std::{fmt::Display, process::exit};

#[derive(Debug, Clone, Copy)]
pub struct Loc<'a> {
    filepath: &'a String,
    line: usize,
    col: usize,
}

impl<'a> Display for Loc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!("{}:{}:{}", self.filepath, self.line, self.col).fmt(f)
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    Exit,
    OParen,
    CParen,
    IntLit(i64),
    Semi,
    Let,
    Ident(String),
    Equal,
    BinOp(OpType),
}

#[derive(Debug, Clone, Copy)]
pub enum OpType {
    Times,
    Divide,
    Modulo,
    BitwiseOr,
    BitwiseAnd,
    Plus,
    Minus,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Exit => "Exit".fmt(f),
            TokenType::OParen => "OpenParenthesis".fmt(f),
            TokenType::CParen => "CloseParenthesis".fmt(f),
            TokenType::IntLit(value) => format!("IntegerLiteral({value})").fmt(f),
            TokenType::Semi => "SemiColon".fmt(f),
            TokenType::Let => "Let".fmt(f),
            TokenType::Ident(name) => format!("Identifier({name})").fmt(f),
            TokenType::Equal => "Equal".fmt(f),
            TokenType::BinOp(OpType::Times) => "Times".fmt(f),
            TokenType::BinOp(OpType::Divide) => "Divide".fmt(f),
            TokenType::BinOp(OpType::Modulo) => "Modulo".fmt(f),
            TokenType::BinOp(OpType::BitwiseOr) => "BitwiseOr".fmt(f),
            TokenType::BinOp(OpType::BitwiseAnd) => "BitwiseAnd".fmt(f),
            TokenType::BinOp(OpType::Plus) => "Plus".fmt(f),
            TokenType::BinOp(OpType::Minus) => "Minus".fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub loc: Loc<'a>,
    pub token_type: TokenType,
}

pub struct Lexer<'a> {
    filepath: &'a String,
    line: usize,
    line_start: usize,
    cursor: usize,
    content: &'a [char],
}

impl<'a> Lexer<'a> {
    pub fn new(filepath: &'a String, content: &'a [char]) -> Self {
        Lexer {
            filepath,
            line: 0,
            line_start: 0,
            cursor: 0,
            content,
        }
    }

    fn get_loc(&self) -> Loc<'a> {
        Loc {
            filepath: self.filepath,
            line: self.line,
            col: self.cursor - self.line_start,
        }
    }

    fn is_empty(&self) -> bool {
        self.cursor >= self.content.len()
    }

    fn strip_left(&mut self) {
        while !self.is_empty() && self.content[self.cursor].is_ascii_whitespace() {
            if self.content[self.cursor] == '\n' {
                self.line += 1;
                self.line_start = self.cursor + 1;
            }
            self.cursor += 1;
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.strip_left();
        if self.is_empty() {
            return None;
        }
        let start = self.cursor;

        if self.content[start] == '\'' {
            self.cursor += 1;
            while !self.is_empty() && self.content[self.cursor] != '\'' {
                self.cursor += 1;
                if self.content[self.cursor - 1] == '\n'
                    || (self.is_empty() && self.content[self.cursor] != '\'')
                {
                    eprintln!("ERROR: {}: Unfinished char literal ", self.get_loc());
                    exit(1);
                }
                if self.content[self.cursor - 1] == '\\' {
                    self.cursor += 1;
                }
            }
            self.cursor += 1;
            if self.cursor - start < 3 {
                eprintln!("ERROR: {}: empty char literal ", self.get_loc());
                eprintln!(
                    "INFO: Got: {} with size {}",
                    self.content[start..self.cursor].iter().collect::<String>(),
                    self.cursor - start
                );
                exit(1);
            }
            let c_lit = self.content[start..self.cursor]
                .iter()
                .collect::<String>()
                .replace("\\n", "\n")
                .replace("\\t", "\t")
                .replace("\\r", "\r")
                .replace("\\'", "'")
                .replace("\\\"", "\"")
                .as_bytes()[1] as i64;

            Some(Token {
                loc: self.get_loc(),
                token_type: TokenType::IntLit(c_lit),
            })
        } else if self.content[start].is_ascii_digit() {
            while !self.is_empty() && self.content[self.cursor].is_ascii_digit() {
                self.cursor += 1;
            }
            Some(Token {
                loc: self.get_loc(),
                token_type: TokenType::IntLit(
                    self.content[start..self.cursor]
                        .iter()
                        .collect::<String>()
                        .parse::<i64>()
                        .unwrap(),
                ),
            })
        } else if self.content[start].is_ascii_alphabetic() || self.content[start] == '_' {
            while !self.is_empty()
                && (self.content[self.cursor].is_ascii_alphanumeric()
                    || self.content[self.cursor] == '_')
            {
                self.cursor += 1;
            }
            let buf = self.content[start..self.cursor].iter().collect::<String>();
            match buf.as_str() {
                "exit" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Exit,
                }),
                "let" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Let,
                }),
                _ => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Ident(buf),
                }),
            }
        } else {
            self.cursor += 1;
            let buf = self.content[start..self.cursor].iter().collect::<String>();
            match buf.as_str() {
                "(" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::OParen,
                }),
                ")" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::CParen,
                }),
                ";" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Semi,
                }),
                "=" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Equal,
                }),
                "*" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::BinOp(OpType::Times),
                }),
                "/" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::BinOp(OpType::Divide),
                }),
                "%" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::BinOp(OpType::Modulo),
                }),
                "|" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::BinOp(OpType::BitwiseOr),
                }),
                "&" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::BinOp(OpType::BitwiseAnd),
                }),
                "+" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::BinOp(OpType::Plus),
                }),
                "-" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::BinOp(OpType::Minus),
                }),
                _ => {
                    eprintln!("ERROR: {}: Unknown symbol `{buf}`", self.get_loc());
                    exit(1);
                }
            }
        }
    }
}
