use std::{
    collections::HashMap,
    io::{Error, Write},
    process::exit,
};

use self::lexer::{OpType, Token, TokenType};

pub mod lexer;

#[derive(Debug, Clone)]
pub enum Arg {
    IntLit(i64),
    Ident(String),
    Paren(Box<Arg>),
    BinOp(OpType, Box<Arg>, Box<Arg>),
}

impl Arg {
    fn wrong_token(first: Token) -> ! {
        eprintln!("ERROR: {} Unexpected token.", first.loc);
        eprintln!("Expected one of: IntegerLiteral, OpenParenthesis, Operator");
        eprintln!("Got: {}", first.token_type);
        exit(1)
    }

    fn parse(tokens: &mut Vec<Token>) -> Option<Self> {
        if tokens.is_empty() {
            None
        } else {
            tokens.rotate_left(1);
            let first = tokens.pop().unwrap();
            let arg1 = match first.clone().token_type {
                TokenType::IntLit(value) => Some(Self::IntLit(value)),
                TokenType::Ident(name) => {
                    if tokens.is_empty() {
                        Some(Self::Ident(name.clone()))
                    } else {
                        if let TokenType::Equal = tokens.get(0).unwrap().token_type {
                            Self::wrong_token(first)
                        } else {
                            Some(Self::Ident(name))
                        }
                    }
                }
                TokenType::OParen => Some(Self::parse_paren(first.clone(), tokens)),
                TokenType::BinOp(_)
                | TokenType::Exit
                | TokenType::Let
                | TokenType::CParen
                | TokenType::Semi
                | TokenType::Equal => Self::wrong_token(first),
            };
            if let TokenType::BinOp(op_type) = tokens.clone().first().unwrap().token_type {
                tokens.rotate_left(1);
                tokens.pop().unwrap();
                Some(Self::parse_bin_op(
                    first.clone(),
                    tokens,
                    op_type,
                    arg1.unwrap(),
                ))
            } else {
                arg1
            }
        }
    }

    fn parse_paren(first: Token, tokens: &mut Vec<Token>) -> Self {
        if let Some(arg) = Self::parse(tokens) {
            if tokens.is_empty() {
                eprintln!("ERROR: {}: Unmatched {}", first.loc, first.token_type);
                exit(1)
            } else {
                tokens.rotate_left(1);
                let last = tokens.pop().unwrap();
                if let TokenType::CParen = last.token_type {
                    Self::Paren(Box::new(arg))
                } else {
                    eprintln!("ERROR: {}: Unmatched {}", first.loc, first.token_type);
                    exit(1)
                }
            }
        } else {
            eprintln!(
                "ERROR: {} Expected an arg after {}",
                first.loc, first.token_type
            );
            exit(1)
        }
    }

    fn parse_bin_op(first: Token, tokens: &mut Vec<Token>, op_type: OpType, arg1: Arg) -> Self {
        if let Some(arg2) = Self::parse(tokens) {
            if let Arg::BinOp(_, _, prev_arg2) = arg1 {
                todo!("Check for mult and divide priority")
            } else {
                todo!("return arg")
            }
        } else {
            eprintln!(
                "ERROR: {} Expected an arg after {}",
                first.loc, first.token_type
            );
            exit(1)
        }
    }

    fn compile(&self, asm: &mut dyn Write, prog: &mut Program) -> Result<usize, Error> {
        match self {
            Arg::IntLit(value) => {
                asm.write(format!("    mov     rax, {value}\n").as_bytes())?;
                asm.write("    push    rax\n".as_bytes())
            }
            Arg::Paren(statement) => statement.compile(asm, prog),
            Arg::Ident(name) => {
                if let Some(offset) = prog.vars.get(name) {
                    asm.write(
                        format!(
                            "    push    QWORD [rsp+{}]\n",
                            (prog.stack_pos - 1 - offset) * 8
                        )
                        .as_bytes(),
                    )
                } else {
                    eprintln!("ERROR: Use of undeclared identifier {name}");
                    exit(1);
                }
            }
            Arg::BinOp(op_type, arg1, arg2) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Exit(Arg),
    VarDecla(String, Arg),
    VarActu(String, Arg),
}

impl Statement {
    fn parse_exit(first: Token, tokens: &mut Vec<Token>) -> Self {
        if let Some(arg) = Arg::parse(tokens) {
            if tokens.is_empty() {
                eprintln!("ERROR: {}: Unmatched {}", first.loc, first.token_type);
                exit(1)
            } else {
                tokens.rotate_left(1);
                let last = tokens.pop().unwrap();
                if let TokenType::Semi = last.token_type {
                    Self::Exit(arg)
                } else {
                    eprintln!(
                        "ERROR: {}: Missing Semicolon at the end of this statement.",
                        first.loc
                    );
                    exit(1)
                }
            }
        } else {
            eprintln!(
                "ERROR: {}: Expected an arg after {}",
                first.loc, first.token_type
            );
            exit(1)
        }
    }

    fn parse_var_decla(first: Token, tokens: &mut Vec<Token>) -> Self {
        if tokens.is_empty() {
            eprintln!(
                "ERROR: {}: Expected indentifier after `let`. Got: Nothing",
                first.loc
            );
            exit(1);
        }
        tokens.rotate_left(1);
        let first = tokens.pop().unwrap();
        if let TokenType::Ident(name) = first.token_type.clone() {
            if tokens.is_empty() {
                eprintln!(
                    "ERROR: {}: Expected `=` after identifier. Got: Nothing",
                    first.loc
                );
                exit(1);
            }
            tokens.rotate_left(1);
            let second = tokens.pop().unwrap();
            if let TokenType::Equal = second.token_type {
                if let Some(arg) = Arg::parse(tokens) {
                    if tokens.is_empty() {
                        eprintln!(
                            "ERROR: {}: Expected `;` after statement. Got: Nothing",
                            second.loc
                        );
                        exit(1);
                    }
                    tokens.rotate_left(1);
                    let last = tokens.pop().unwrap();
                    if let TokenType::Semi = last.token_type {
                        Self::VarDecla(name, arg)
                    } else {
                        eprintln!(
                            "ERROR: {}: Expected `;` after statement. Got: {}",
                            last.loc, last.token_type
                        );
                        exit(1);
                    }
                } else {
                    eprintln!("ERROR: {}: Expected a statement after `=`", first.loc);
                    exit(1)
                }
            } else {
                eprintln!(
                    "ERROR: {}: Expected `=` after identifier. Got: {}",
                    first.loc, first.token_type
                );
                exit(1);
            }
        } else {
            eprintln!(
                "ERROR: {}: Expected indentifier after `let`. Got: {}",
                first.loc, first.token_type
            );
            exit(1);
        }
    }

    fn parse_var_actu(first: Token, tokens: &mut Vec<Token>, name: String) -> Self {
        tokens.rotate_left(1);
        tokens.pop().unwrap();
        if let Some(arg) = Arg::parse(tokens) {
            if tokens.is_empty() {
                eprintln!(
                    "ERROR: {}: Expected `;` after statement. Got: Nothing.",
                    first.loc
                );
                exit(1);
            }
            tokens.rotate_left(1);
            let last = tokens.pop().unwrap();
            if let TokenType::Semi = last.token_type {
                Self::VarActu(name, arg)
            } else {
                eprintln!(
                    "ERROR: {}: Expected `;` after statement. Got: {}.",
                    last.loc, last.token_type
                );
                exit(1);
            }
        } else {
            eprintln!("ERROR: {}: Expected statement after `=`", first.loc);
            exit(1);
        }
    }

    fn wrong_token(first: Token) -> ! {
        eprintln!("ERROR: {} Unexpected token.", first.loc);
        eprintln!("Expected one of: Let, Identifier, Exit");
        eprintln!("Got: {}", first.token_type);
        exit(1)
    }

    pub fn parse(tokens: &mut Vec<Token>) -> Option<Self> {
        if tokens.is_empty() {
            None
        } else {
            tokens.rotate_left(1);
            let first = tokens.pop().unwrap();
            match first.clone().token_type {
                TokenType::Exit => Some(Self::parse_exit(first.clone(), tokens)),
                TokenType::Let => Some(Self::parse_var_decla(first.clone(), tokens)),
                TokenType::Ident(name) => {
                    if tokens.is_empty() {
                        Self::wrong_token(first)
                    } else {
                        if let TokenType::Equal = tokens.get(0).unwrap().token_type {
                            Some(Self::parse_var_actu(first, tokens, name))
                        } else {
                            Self::wrong_token(first)
                        }
                    }
                }
                TokenType::OParen
                | TokenType::IntLit(_)
                | TokenType::BinOp(_)
                | TokenType::CParen
                | TokenType::Semi
                | TokenType::Equal => Self::wrong_token(first),
            }
        }
    }

    fn compile(&self, asm: &mut dyn Write, prog: &mut Program) -> Result<usize, Error> {
        match self {
            Statement::Exit(statement) => {
                statement.compile(asm, prog)?;
                asm.write("    mov     rax, 60\n".as_bytes())?;
                asm.write("    pop     rdi\n".as_bytes())?;
                asm.write("    syscall\n".as_bytes())
            }
            Statement::VarDecla(name, statement) => {
                if let Some(_) = prog.vars.get(name) {
                    eprintln!("ERROR: Variable {name} already declared");
                    exit(1);
                }
                prog.vars.insert(name.to_string(), prog.stack_pos);
                prog.stack_pos += 1;
                statement.compile(asm, prog)
            }
            Statement::VarActu(name, statement) => {
                if let Some(offset) = prog.clone().vars.get(name) {
                    statement.compile(asm, prog)?;
                    asm.write("    pop     rax\n".as_bytes())?;
                    asm.write(
                        format!(
                            "    mov     QWORD [rsp+{}], rax\n",
                            (prog.stack_pos - 1 - offset) * 8
                        )
                        .as_bytes(),
                    )
                } else {
                    eprintln!("ERROR: Use of undeclared identifier {name}");
                    exit(1);
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub stack_pos: usize,
    pub vars: HashMap<String, usize>,
}

impl Program {
    pub fn parse(tokens: &mut Vec<Token>) -> Self {
        let mut statements: Vec<Statement> = vec![];
        let vars: HashMap<String, usize> = HashMap::new();
        let stack_pos: usize = 0;
        while let Some(statement) = Statement::parse(tokens) {
            statements.push(statement);
        }
        Self {
            statements,
            stack_pos,
            vars,
        }
    }

    pub fn compile(mut self, asm: &mut dyn Write) -> Result<usize, Error> {
        asm.write("global _start\n\n_start:\n".as_bytes())?;
        for statement in self.clone().statements {
            statement.compile(asm, &mut self)?;
        }
        asm.write("    mov     rax, 60\n".as_bytes())?;
        asm.write("    mov     rdi, 0\n".as_bytes())?;
        asm.write("    syscall\n".as_bytes())
    }
}
