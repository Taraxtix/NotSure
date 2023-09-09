//#region Imports
use std::{fmt::Display, process::exit};
//#endregion

//#region Definitions

pub trait Queue {
    type Item;
    fn q_is_empty(&self) -> bool;
    fn q_peek(&self) -> Option<&Self::Item>;
    fn q_pop(&mut self) -> Option<Self::Item>;
    fn q_pop_if(&mut self, pred: impl Fn(&Self::Item) -> bool) -> Option<Self::Item> {
        if !pred(self.q_peek()?) {
            None
        } else {
            self.q_pop()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Loc {
    filepath: String,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone)]
pub enum TokenType {
    Exit,
    OParen,
    CParen,
    IntLit(u64),
    StringLit(String),
    Semi,
    Let,
    Ident(String),
    Equal,
    BinOp(OpType),
    Address,
    OCurly,
    CCurly,
    If,
    Else,
    While,
    Do,
    Dbg,
    Syscall,
    Comma,
    Dereference,
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
    Equal,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub loc: Loc,
    pub token_type: TokenType,
}

pub struct Lexer<'a> {
    filepath: String,
    line: usize,
    line_start: usize,
    cursor: usize,
    content: &'a [char],
}
//#endregion

//#region Implementations
pub fn exit_msg(msg: String) -> ! {
    eprintln!("ERROR: {}", msg);
    exit(1);
}

impl<'a> Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!("{}:{}:{}", self.filepath, self.line, self.col).fmt(f)
    }
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
            TokenType::BinOp(OpType::Equal) => "Equal".fmt(f),
            TokenType::BinOp(OpType::Greater) => "Greater".fmt(f),
            TokenType::BinOp(OpType::Less) => "Less".fmt(f),
            TokenType::BinOp(OpType::GreaterEqual) => "GreaterEqual".fmt(f),
            TokenType::BinOp(OpType::LessEqual) => "LessEqual".fmt(f),
            TokenType::BinOp(OpType::LogicalAnd) => "LogicalAnd".fmt(f),
            TokenType::BinOp(OpType::LogicalOr) => "Minus".fmt(f),
            TokenType::Address => "Address".fmt(f),
            TokenType::OCurly => "OpenCurlyBraces".fmt(f),
            TokenType::CCurly => "CloseCurlyBraces".fmt(f),
            TokenType::If => "If".fmt(f),
            TokenType::Else => "Else".fmt(f),
            TokenType::While => "While".fmt(f),
            TokenType::Dbg => "Dbg".fmt(f),
            TokenType::Syscall => "Syscall".fmt(f),
            TokenType::Comma => "Comma".fmt(f),
            TokenType::Do => "Do".fmt(f),
            TokenType::StringLit(_) => "StringLiteral".fmt(f),
            TokenType::Dereference => "Dereference".fmt(f),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(filepath: String, content: &'a [char]) -> Self {
        Lexer {
            filepath: filepath.clone(),
            line: 0,
            line_start: 0,
            cursor: 0,
            content,
        }
    }

    fn get_loc(&self) -> Loc {
        Loc {
            filepath: self.filepath.clone(),
            line: self.line,
            col: self.cursor - self.line_start,
        }
    }

    fn first_is<F>(&self, pred: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        !self.q_is_empty() && pred(self.content[self.cursor])
    }

    fn strip_left(&mut self) {
        while self.first_is(|c| c.is_ascii_whitespace()) {
            if self.first_is(|c| c == '\n') {
                self.line += 1;
                self.line_start = self.cursor + 1;
            }
            self.cursor += 1;
        }
    }
}

impl<'a> Queue for Lexer<'a> {
    type Item = char;

    fn q_is_empty(&self) -> bool {
        self.cursor >= self.content.len()
    }

    fn q_peek(&self) -> Option<&Self::Item> {
        self.content.get(self.cursor)
    }

    fn q_pop(&mut self) -> Option<Self::Item> {
        let c = self.content.get(self.cursor)?;
        self.cursor += 1;
        Some(*c)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.strip_left();
        if self.q_is_empty() {
            return None;
        }
        let start = self.cursor;

        if let Some(_) = self.q_pop_if(|c| *c == '\"') {
            //String literal
            while let Some(c) = self.q_pop() {
                if c == '\"' {
                    break;
                }
                if c == '\n' || self.q_is_empty() {
                    eprintln!("ERROR: {}: Unfinished char literal ", self.get_loc());
                    exit(1);
                }
                self.q_pop_if(|c| *c == '\\');
            }
            let str_lit = self.content[start..self.cursor - 1]
                .iter()
                .collect::<String>()
                .replace("\\n", "\n")
                .replace("\\t", "\t")
                .replace("\\r", "\r")
                .replace("\\'", "'")
                .replace("\\\"", "\"");

            return Some(Token {
                loc: self.get_loc(),
                token_type: TokenType::StringLit(str_lit),
            });
        }
        if let Some(_) = self.q_pop_if(|c| *c == '\'') {
            //Char literal
            while let Some(c) = self.q_peek() {
                if *c == '\'' {
                    break;
                }
                if *c == '\n' || self.q_is_empty() {
                    eprintln!("ERROR: {}: Unfinished char literal ", self.get_loc());
                    exit(1);
                }
                self.q_pop_if(|c| *c == '\\');
            }
            self.q_pop();
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
                .as_bytes()[1] as u64;

            return Some(Token {
                loc: self.get_loc(),
                token_type: TokenType::IntLit(c_lit),
            });
        }
        if let Some(_) = self.q_pop_if(|c| c.is_ascii_digit()) {
            //Integer literal
            while let Some(_) = self.q_pop_if(|c| c.is_ascii_digit()) {}
            return Some(Token {
                loc: self.get_loc(),
                token_type: TokenType::IntLit(
                    self.content[start..self.cursor]
                        .iter()
                        .collect::<String>()
                        .parse::<u64>()
                        .unwrap(),
                ),
            });
        }
        if let Some(_) = self.q_pop_if(|c| c.is_ascii_alphabetic() || *c == '_') {
            //Multi-character identifier
            while let Some(_) = self.q_pop_if(|c| c.is_ascii_alphabetic() || *c == '_') {}
            let buf = self.content[start..self.cursor].iter().collect::<String>();
            return match buf.as_str() {
                "syscall" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Syscall,
                }),
                "exit" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Exit,
                }),
                "let" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Let,
                }),
                "if" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::If,
                }),
                "else" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Else,
                }),
                "dbg" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Dbg,
                }),
                "while" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::While,
                }),
                _ => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Ident(buf),
                }),
            };
        }
        {
            //Single-character identifier
            self.q_pop();
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
                "=" => {
                    if let Some(_) = self.q_pop_if(|c| *c == '=') {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::BinOp(OpType::Equal),
                        })
                    } else {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::Equal,
                        })
                    }
                }
                ">" => {
                    if let Some(_) = self.q_pop_if(|c| *c == '=') {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::BinOp(OpType::GreaterEqual),
                        })
                    } else {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::BinOp(OpType::Greater),
                        })
                    }
                }
                "<" => {
                    if let Some(_) = self.q_pop_if(|c| *c == '=') {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::BinOp(OpType::LessEqual),
                        })
                    } else {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::BinOp(OpType::Less),
                        })
                    }
                }
                "*" => {
                    if let Some(_) = self.q_pop_if(|c| *c == ':') {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::Dereference,
                        })
                    } else {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::BinOp(OpType::Times),
                        })
                    }
                }
                "/" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::BinOp(OpType::Divide),
                }),
                "%" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::BinOp(OpType::Modulo),
                }),
                "|" => {
                    if let Some(_) = self.q_pop_if(|c| *c == '|') {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::BinOp(OpType::LogicalOr),
                        })
                    } else {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::BinOp(OpType::BitwiseOr),
                        })
                    }
                }
                "&" => {
                    if let Some(_) = self.q_pop_if(|c| *c == '&') {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::BinOp(OpType::LogicalAnd),
                        })
                    } else if let Some(_) = self.q_pop_if(|c| *c == ':') {
                        Some(Token {
                            token_type: TokenType::Address,
                            loc: self.get_loc(),
                        })
                    } else {
                        Some(Token {
                            loc: self.get_loc(),
                            token_type: TokenType::BinOp(OpType::BitwiseAnd),
                        })
                    }
                }
                "+" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::BinOp(OpType::Plus),
                }),
                "-" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::BinOp(OpType::Minus),
                }),
                "{" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::OCurly,
                }),
                "}" => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::CCurly,
                }),
                "," => Some(Token {
                    loc: self.get_loc(),
                    token_type: TokenType::Comma,
                }),
                _ => {
                    eprintln!("ERROR: {}: Unknown symbol `{buf}`", self.get_loc());
                    exit(1);
                }
            }
        }
    }
}
//#endregion
