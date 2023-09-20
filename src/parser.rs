//#region Imports
use std::collections::hash_map;
use std::{
    collections::HashMap,
    io::{Error, Write},
    process::exit,
};

use self::lexer::{exit_msg, OpType, Queue, Token, TokenType};

pub mod lexer;
//#endregion

//#region Definitions
#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub stack_pos: usize,
    pub vars: Vec<HashMap<String, usize>>,
    pub str_literals: HashMap<String, usize>,
    pub str_literals_size: usize,
    pub cf_id: usize,
}

#[derive(Debug, Clone)]
pub enum Arg {
    IntLit(u64),
    StringLit(String),
    Ident(String),
    Paren(Box<Arg>),
    BinOp(OpType, Box<Arg>, Box<Arg>),
    Address(String),
    Dereference(u8, Box<Arg>),
    Syscall(Vec<Arg>),
}

#[derive(Debug, Clone)]
pub enum CfType {
    If(Box<Arg>, Box<Statement>),
    IfElse(Box<Arg>, Box<Statement>, Box<Statement>),
    While(Box<Arg>, Box<Statement>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Exit(Arg),
    VarDecla(String, Arg),
    VarActu(String, Arg),
    Scope(Vec<Statement>),
    ControlFlow(CfType),
    Dbg(Arg),
    DereferenceActu(u8, Arg, Arg),
    Drop(Arg),
}
//#endregion

//#region Implementations
impl Queue for Vec<Token> {
    type Item = Token;

    fn q_is_empty(&self) -> bool {
        self.is_empty()
    }

    fn q_peek(&self) -> Option<&Self::Item> {
        self.first()
    }

    fn q_pop(&mut self) -> Option<Self::Item> {
        if self.is_empty() {
            None
        } else {
            self.rotate_left(1);
            self.pop()
        }
    }

    fn q_pop_if(&mut self, pred: impl Fn(&Self::Item) -> bool) -> Option<Self::Item> {
        if self.is_empty() || !pred(self.first().unwrap()) {
            None
        } else {
            self.rotate_left(1);
            self.pop()
        }
    }
}

impl Arg {
    fn wrong_token(first: Token) -> ! {
        eprintln!("ERROR: {} Unexpected token.", first.loc);
        eprintln!("Expected one of: IntegerLiteral, OpenParenthesis, Operator");
        eprintln!("Got: {}", first.token_type);
        exit(1)
    }

    fn parse(tokens: &mut Vec<Token>) -> Option<Self> {
        let first = tokens.q_pop()?;
        let arg1 = match first.clone().token_type {
            TokenType::IntLit(value) => Some(Self::IntLit(value)),
            TokenType::Ident(name) => {
                if tokens
                    .q_pop_if(|tok| matches!(tok.token_type, TokenType::Equal))
                    .is_some()
                {
                    Self::wrong_token(first)
                } else {
                    Some(Self::Ident(name))
                }
            }
            TokenType::OParen => Some(Self::parse_paren(first.clone(), tokens)),
            TokenType::StringLit(str_lit) => Some(Self::StringLit(str_lit)),
            TokenType::Dereference(size) => Some(Self::parse_deref(first.clone(), tokens, size)),
            TokenType::Address => {
                if let Some(Arg::Ident(name)) = Self::parse(tokens) {
                    Some(Arg::Address(name))
                } else {
                    eprintln!(
                        "ERROR: {}: Address operator must be followed by an identifier",
                        first.loc
                    );
                    exit(1);
                }
            }
            TokenType::Syscall => Some(Self::parse_syscall(first.clone(), tokens)),
            _ => Self::wrong_token(first),
        };
        if let Some(TokenType::BinOp(op_type)) = tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::BinOp(_)))
            .map(|tok| tok.token_type)
        {
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

    fn parse_deref(first: Token, tokens: &mut Vec<Token>, size: u8) -> Self {
        tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::OParen))
            .unwrap_or_else(|| exit_msg(format!("ERROR: expected open paren after {}", first.loc)));
        let arg = Self::parse(tokens)
            .unwrap_or_else(|| exit_msg(format!("ERROR: expected arg after {}", first.loc)));
        tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::CParen))
            .unwrap_or_else(|| {
                exit_msg(format!("ERROR: expected close paren after {}", first.loc))
            });
        Self::Dereference(size, Box::new(arg))
    }

    fn parse_paren(first: Token, tokens: &mut Vec<Token>) -> Self {
        if let Some(arg) = Self::parse(tokens) {
            if let Some(TokenType::CParen) = tokens.q_pop().map(|tok| tok.token_type) {
                Self::Paren(Box::new(arg))
            } else {
                eprintln!("ERROR: {}: Unmatched {}", first.loc, first.token_type);
                exit(1)
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
            if let Arg::BinOp(op_type_1, arg1_1, arg1_2) = arg1.clone() {
                match op_type {
                    OpType::Times | OpType::Divide => Arg::BinOp(
                        op_type_1,
                        arg1_1,
                        Box::new(Arg::BinOp(op_type, arg1_2, Box::new(arg2))),
                    ),
                    _ => Arg::BinOp(op_type, Box::new(arg1), Box::new(arg2)),
                }
            } else if let Arg::BinOp(op_type_2, arg2_1, arg2_2) = arg2.clone() {
                match op_type {
                    OpType::Times | OpType::Divide => Arg::BinOp(
                        op_type_2,
                        Box::new(Arg::BinOp(op_type, Box::new(arg1), arg2_1)),
                        arg2_2,
                    ),
                    _ => Arg::BinOp(op_type, Box::new(arg1), Box::new(arg2)),
                }
            } else {
                Arg::BinOp(op_type, Box::new(arg1), Box::new(arg2))
            }
        } else {
            eprintln!(
                "ERROR: {} Expected an arg after {}",
                first.loc, first.token_type
            );
            exit(1)
        }
    }

    fn parse_syscall(first: Token, tokens: &mut Vec<Token>) -> Self {
        if tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::OParen))
            .is_none()
        {
            exit_msg(format!(
                "ERROR: {}: Expected open parenthesis after {}",
                first.loc, first.token_type
            ))
        }
        let mut args: Vec<Arg> = vec![];
        while tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::CParen))
            .is_none()
        {
            args.push(Self::parse(tokens).unwrap_or_else(|| {
                exit_msg(format!(
                    "ERROR: {}: Expected an argument for `{}`",
                    first.loc, first.token_type
                ))
            }));
        }
        Self::Syscall(args)
    }

    fn compile(&self, asm: &mut dyn Write, prog: &mut Program) -> Result<usize, Error> {
        match self {
            Self::IntLit(value) => prog.push(asm, format!("{value}")),
            Self::Paren(arg) => arg.compile(asm, prog),
            Self::Ident(name) => {
                let offset = prog.find_var(name.to_string());
                let _ = asm.write(format!(";; Ident {name}\n").as_bytes())?;
                prog.push(
                    asm,
                    format!("QWORD [rsp+{}]", (prog.stack_pos - offset) * 8),
                )
            }
            Self::Address(name) => {
                let offset = prog.find_var(name.to_string());
                let _ = asm.write(";; Address\n".as_bytes())?;
                let _ = asm.write(
                    format!("    lea     rax, [rsp+{}]\n", (prog.stack_pos - offset) * 8)
                        .as_bytes(),
                )?;
                prog.push(asm, "rax".to_string())
            }
            Self::StringLit(str_lit) => {
                let idx = if let Some(val) = prog.str_literals.get(str_lit) {
                    *val
                } else {
                    prog.str_literals_size += 1;
                    prog.str_literals
                        .insert(str_lit.clone(), prog.str_literals_size);
                    prog.str_literals_size
                };
                let _ = asm.write(";; StringLit\n".as_bytes())?;
                let _ = asm.write(format!("    mov     rax, STR_{}\n", idx).as_bytes())?;
                prog.push(asm, "rax".to_string())
            }
            Self::BinOp(op_type, arg1, arg2) => {
                let _ = asm.write(";; BinOp\n".as_bytes())?;
                arg2.compile(asm, prog)?;
                arg1.compile(asm, prog)?;
                prog.pop(asm, "rax".to_string())?;
                prog.pop(asm, "rbx".to_string())?;
                match op_type {
                    OpType::Times => {
                        let _ = asm.write(";; Times\n".as_bytes())?;
                        asm.write("    mul     rbx\n".as_bytes())?
                    }
                    OpType::Divide => {
                        let _ = asm.write(";; Divide\n".as_bytes())?;
                        let _ = asm.write("    mov     rdx, 0\n".as_bytes())?;
                        asm.write("    div     rbx\n".as_bytes())?
                    }
                    OpType::Modulo => {
                        let _ = asm.write(";; Modulo\n".as_bytes())?;
                        let _ = asm.write("    mov     rdx, 0\n".as_bytes())?;
                        let _ = asm.write("    div     rbx\n".as_bytes())?;
                        asm.write("    mov     rax, rdx\n".as_bytes())?
                    }
                    OpType::BitwiseOr => {
                        let _ = asm.write(";; BitwiseOr\n".as_bytes())?;
                        asm.write("    or      rax, rbx\n".as_bytes())?
                    }
                    OpType::BitwiseAnd => {
                        let _ = asm.write(";; BitwiseAnd\n".as_bytes())?;
                        asm.write("    and     rax, rbx\n".as_bytes())?
                    }
                    OpType::Plus => {
                        let _ = asm.write(";; Plus\n".as_bytes())?;
                        asm.write("    add     rax, rbx\n".as_bytes())?
                    }
                    OpType::Minus => {
                        let _ = asm.write(";; Minus\n".as_bytes())?;
                        asm.write("    sub     rax, rbx\n".as_bytes())?
                    }
                    OpType::Equal => {
                        let _ = asm.write(";; Equal\n".as_bytes())?;
                        let _ = asm.write("    mov     rcx, 1\n".as_bytes())?;
                        let _ = asm.write("    cmp     rax, rbx\n".as_bytes())?;
                        let _ = asm.write("    mov     rax, 0\n".as_bytes())?;
                        asm.write("    cmove   rax, rcx\n".as_bytes())?
                    }
                    OpType::Greater => {
                        let _ = asm.write(";; Greater\n".as_bytes())?;
                        let _ = asm.write("    mov     rcx, 1\n".as_bytes())?;
                        let _ = asm.write("    cmp     rax, rbx\n".as_bytes())?;
                        let _ = asm.write("    mov     rax, 0\n".as_bytes())?;
                        asm.write("    cmovg   rax, rcx\n".as_bytes())?
                    }
                    OpType::Less => {
                        let _ = asm.write(";; Less\n".as_bytes())?;
                        let _ = asm.write("    mov     rcx, 1\n".as_bytes())?;
                        let _ = asm.write("    cmp     rax, rbx\n".as_bytes())?;
                        let _ = asm.write("    mov     rax, 0\n".as_bytes())?;
                        asm.write("    cmovl   rax, rcx\n".as_bytes())?
                    }
                    OpType::GreaterEqual => {
                        let _ = asm.write(";; GreaterEqual\n".as_bytes())?;
                        let _ = asm.write("    mov     rcx, 1\n".as_bytes())?;
                        let _ = asm.write("    cmp     rax, rbx\n".as_bytes())?;
                        let _ = asm.write("    mov     rax, 0\n".as_bytes())?;
                        asm.write("    cmovge  rax, rcx\n".as_bytes())?
                    }
                    OpType::LessEqual => {
                        let _ = asm.write(";; LessEqual\n".as_bytes())?;
                        let _ = asm.write("    mov     rcx, 1\n".as_bytes())?;
                        let _ = asm.write("    cmp     rax, rbx\n".as_bytes())?;
                        let _ = asm.write("    mov     rax, 0\n".as_bytes())?;
                        asm.write("    cmovle  rax, rcx\n".as_bytes())?
                    }
                    OpType::LogicalAnd => {
                        let _ = asm.write(";; Logical AND\n".as_bytes())?;
                        let _ = asm.write("    mov     rcx, 0\n".as_bytes())?;
                        let _ = asm.write("    and     rax, rbx\n".as_bytes())?;
                        let _ = asm.write("    test    rax, rax\n".as_bytes())?;
                        let _ = asm.write("    mov     rax, 1\n".as_bytes())?;
                        asm.write("    cmove   rax, rcx\n".as_bytes())?
                    }
                    OpType::LogicalOr => {
                        let _ = asm.write(";; Logical OR\n".as_bytes())?;
                        let _ = asm.write("    mov     rcx, 0\n".as_bytes())?;
                        let _ = asm.write("    or      rax, rbx\n".as_bytes())?;
                        let _ = asm.write("    test    rax, rax\n".as_bytes())?;
                        let _ = asm.write("    mov     rax, 1\n".as_bytes())?;
                        asm.write("    cmove   rax, rcx\n".as_bytes())?
                    }
                    OpType::LShift => {
                        let _ = asm.write(";; LSHIFT\n".as_bytes())?;
                        let _ = asm.write("    mov     cl, bl\n".as_bytes())?;
                        asm.write("    shl     rax, cl\n".as_bytes())?
                    }
                    OpType::RShift => {
                        let _ = asm.write(";; RSHIFT\n".as_bytes())?;
                        let _ = asm.write("    mov     cl, bl\n".as_bytes())?;
                        asm.write("    shr     rax, cl\n".as_bytes())?
                    }
                };
                prog.push(asm, "rax".to_string())
            }
            Self::Dereference(size, address) => {
                let _ = asm.write(";; Dereference(ARG)\n".as_bytes())?;
                address.compile(asm, prog)?;
                prog.pop(asm, "rbx".to_string())?;
                let _ = asm.write("    xor     rax, rax\n".as_bytes())?;
                match size {
                    64 => asm.write("    mov     rax, QWORD [rbx]\n".as_bytes()),
                    32 => asm.write("    mov     eax, DWORD [rbx]\n".as_bytes()),
                    16 => asm.write("    mov     ax, WORD [rbx]\n".as_bytes()),
                    8 => asm.write("    mov     al, BYTE [rbx]\n".as_bytes()),
                    _ => unreachable!(),
                }?;
                prog.push(asm, "rax".to_string())
            }
            Self::Syscall(args) => {
                let _ = asm.write(";; Syscall\n".as_bytes())?;
                let regs: [&str; 7] = ["rax", "rdi", "rsi", "rdx", "r10", "r8", "r9"];
                for idx in 0..args.len() {
                    args.get(idx).unwrap().compile(asm, prog)?;
                }
                for idx in (0..args.len()).rev() {
                    prog.pop(asm, regs[idx].to_string())?;
                }
                let _ = asm.write("    syscall\n".as_bytes())?;
                prog.push(asm, "rax".to_string())
            }
        }
    }
}

impl CfType {
    fn parse(first: Token, tokens: &mut Vec<Token>) -> CfType {
        let cf_arg: Box<Arg> = Box::new(Arg::parse(tokens).unwrap_or_else(|| {
            exit_msg(format!(
                "{} Expected condition for `{}`",
                first.loc, first.token_type
            ));
        }));

        if tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::Do))
            .is_none()
        {
            exit_msg(format!(
                "{} Expected do after `{}`",
                first.loc, first.token_type
            ))
        }

        let cf_statement: Box<Statement> =
            Box::new(Statement::parse(tokens).unwrap_or_else(|| {
                exit_msg(format!(
                    "{} Expected a statement after `{}`",
                    first.loc, first.token_type
                ))
            }));

        if matches!(first.token_type, TokenType::While) {
            return Self::While(cf_arg, cf_statement);
        }

        if tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::Else))
            .is_some()
        {
            return Self::IfElse(
                cf_arg,
                cf_statement,
                Box::new(Statement::parse(tokens).unwrap_or_else(|| {
                    exit_msg(format!(
                        "ERROR: {}: Expected a statement after `{}`",
                        first.loc,
                        TokenType::Else
                    ))
                })),
            );
        }
        Self::If(cf_arg, cf_statement)
    }

    pub fn compile(&self, asm: &mut dyn Write, prog: &mut Program) -> Result<usize, Error> {
        let cf_id = prog.cf_id;
        prog.cf_id += 1;
        match self {
            CfType::If(arg, scope) => {
                arg.compile(asm, prog)?;
                prog.pop(asm, "rax".to_string())?;
                let _ = asm.write("    test    rax, rax\n".as_bytes())?;
                let _ = asm.write(format!("    je      END_{cf_id}\n").as_bytes())?;
                scope.compile(asm, prog)?;
                asm.write(format!("END_{cf_id}:\n").as_bytes())
            }
            CfType::IfElse(arg, main_scope, else_scope) => {
                let _ = asm.write(format!("START_{cf_id}:\n").as_bytes())?;
                arg.compile(asm, prog)?;
                prog.pop(asm, "rax".to_string())?;
                let _ = asm.write("    test    rax, rax\n".as_bytes())?;
                let _ = asm.write(format!("    je      ELSE_{cf_id}\n").as_bytes())?;
                main_scope.compile(asm, prog)?;
                let _ = asm.write(format!("    jmp     END_{cf_id}\n").as_bytes())?;
                let _ = asm.write(format!("ELSE_{cf_id}:\n").as_bytes())?;
                else_scope.compile(asm, prog)?;
                asm.write(format!("END_{cf_id}:\n").as_bytes())
            }
            CfType::While(arg, scope) => {
                let _ = asm.write(format!("START_{cf_id}:\n").as_bytes())?;
                arg.compile(asm, prog)?;
                prog.pop(asm, "rax".to_string())?;
                let _ = asm.write("    test    rax, rax\n".as_bytes())?;
                let _ = asm.write(format!("    je      END_{cf_id}\n").as_bytes())?;
                scope.compile(asm, prog)?;
                let _ = asm.write(format!("    jmp     START_{cf_id}\n").as_bytes())?;
                asm.write(format!("END_{cf_id}:\n").as_bytes())
            }
        }
    }
}

impl Statement {
    pub fn parse(tokens: &mut Vec<Token>) -> Option<Self> {
        let first = tokens.q_pop()?;
        match first.clone().token_type {
            TokenType::Exit => Some(Self::parse_exit(first.clone(), tokens)),

            TokenType::Let => Some(Self::parse_var_decla(first.clone(), tokens)),

            TokenType::Ident(name) => {
                if let Some(TokenType::Equal) = tokens.first().map(|tok| tok.clone().token_type) {
                    Some(Self::parse_var_actu(first, tokens, name))
                } else {
                    Self::wrong_token(first)
                }
            }

            TokenType::If | TokenType::While => {
                Some(Self::ControlFlow(CfType::parse(first, tokens)))
            }

            TokenType::OCurly => Some(Self::parse_scope(first, tokens)),

            TokenType::Dbg => Some(Self::parse_dbg(first.clone(), tokens)),

            TokenType::Dereference(_) => Some(Self::parse_dereference_actu(first, tokens)),

            _ => {
                tokens.push(first.clone());
                tokens.rotate_right(1);
                Some(Self::parse_drop(first, tokens))
            }
        }
    }

    fn parse_dereference_actu(first: Token, tokens: &mut Vec<Token>) -> Self {
        let address = Arg::parse(tokens).unwrap_or_else(|| {
            exit_msg(format!(
                "{} Expected an argument for `{}`",
                first.loc, first.token_type
            ))
        });
        if tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::Equal))
            .is_none()
        {
            exit_msg(format!(
                "{} Expected an equal sign after `{}`",
                first.loc, first.token_type
            ));
        }
        let value = Arg::parse(tokens).unwrap_or_else(|| {
            exit_msg(format!(
                "{} Expected an argument for `{}`",
                first.loc, first.token_type
            ))
        });
        if tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::Semi))
            .is_none()
        {
            exit_msg(format!(
                "{} Expected a semicolon after `{}`",
                first.loc, first.token_type
            ))
        }
        let size = match first.token_type {
            TokenType::Dereference(size) => size,
            _ => unreachable!(),
        };
        Self::DereferenceActu(size, address, value)
    }

    fn parse_drop(first: Token, tokens: &mut Vec<Token>) -> Self {
        let arg = Arg::parse(tokens)
            .unwrap_or_else(|| exit_msg(format!("{} Expected an argument.", first.loc)));
        if tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::Semi))
            .is_none()
        {
            exit_msg(format!(
                "{} Expected a semicolon after drop statement.",
                first.loc
            ))
        }
        Self::Drop(arg)
    }

    fn parse_scope(open: Token, tokens: &mut Vec<Token>) -> Self {
        let mut statements: Vec<Statement> = vec![];
        if tokens.is_empty() {
            eprintln!("ERROR: {} Unmatched `{}`", open.loc, '{');
            exit(1);
        }
        let mut first = tokens.first().unwrap().clone();
        while !matches!(first.token_type, TokenType::CCurly) {
            statements.push(Self::parse(tokens).unwrap());

            if tokens.is_empty() {
                eprintln!("ERROR: {} Unmatched `{}`", first.loc, '{');
                exit(1);
            } else {
                first = tokens.first().unwrap().clone();
            }
        }
        tokens.q_pop();
        Self::Scope(statements)
    }

    fn parse_exit(first: Token, tokens: &mut Vec<Token>) -> Self {
        let arg = Arg::parse(tokens).unwrap_or_else(|| {
            exit_msg(format!(
                "ERROR: {}: Expected an arg after {}",
                first.loc, first.token_type
            ))
        });
        if tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::Semi))
            .is_none()
        {
            exit_msg(format!("ERROR: {}: Missing Semicolon", first.loc))
        }
        Self::Exit(arg)
    }

    fn parse_dbg(first: Token, tokens: &mut Vec<Token>) -> Self {
        let arg = Arg::parse(tokens).unwrap_or_else(|| {
            exit_msg(format!(
                "ERROR: {}: Expected an arg after {}",
                first.loc, first.token_type
            ))
        });
        if tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::Semi))
            .is_none()
        {
            exit_msg(format!("ERROR: {}: Missing Semicolon", first.loc))
        }
        Self::Dbg(arg)
    }

    fn parse_var_decla(first: Token, tokens: &mut Vec<Token>) -> Self {
        let (ident, name) = if let Some(TokenType::Ident(name)) =
            tokens.q_peek().map(|tok| tok.clone().token_type)
        {
            (tokens.q_pop().unwrap(), name.to_string())
        } else {
            exit_msg(format!(
                "ERROR: {}: Expected identifier after `let`.",
                first.clone().loc,
            ))
        };

        if tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::Equal))
            .is_none()
        {
            exit_msg(format!(
                "ERROR: {}: Expected `=` after identifier.",
                ident.loc
            ))
        }
        let arg = Arg::parse(tokens).unwrap_or_else(|| {
            exit_msg(format!("ERROR: {}: Expected an arg after `=`", first.loc))
        });
        if tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::Semi))
            .is_none()
        {
            exit_msg(format!(
                "ERROR: {}: Expected `;` after statement.",
                first.loc
            ))
        }
        Self::VarDecla(name, arg)
    }

    fn parse_var_actu(first: Token, tokens: &mut Vec<Token>, name: String) -> Self {
        tokens.q_pop();
        let arg = Arg::parse(tokens).unwrap_or_else(|| {
            exit_msg(format!(
                "ERROR: {}: Expected statement after `=`",
                first.loc
            ))
        });
        if tokens
            .q_pop_if(|tok| matches!(tok.token_type, TokenType::Semi))
            .is_none()
        {
            exit_msg(format!(
                "ERROR: {}: Expected `;` after statement.",
                first.clone().loc
            ))
        }
        Self::VarActu(name, arg)
    }

    fn wrong_token(first: Token) -> ! {
        eprintln!("ERROR: {} Unexpected token.", first.loc);
        eprintln!("Expected one of: Let, Identifier, Exit");
        eprintln!("Got: {}", first.token_type);
        exit(1)
    }

    fn compile(&self, asm: &mut dyn Write, prog: &mut Program) -> Result<usize, Error> {
        match self {
            Statement::Exit(arg) => {
                let _ = asm.write(";; Exit\n".as_bytes())?;
                arg.compile(asm, prog)?;
                let _ = asm.write("    mov     rax, 60\n".as_bytes())?;
                prog.pop(asm, "rdi".to_string())?;
                asm.write("    syscall\n".as_bytes())
            }
            Statement::VarDecla(name, statement) => {
                let _ = asm.write(format!(";; VarDecla {name}\n").as_bytes())?;
                prog.create_var(name.to_string());
                statement.compile(asm, prog)
            }
            Statement::VarActu(name, statement) => {
                let _ = asm.write(format!(";; VarActu {name}\n").as_bytes())?;
                let offset = prog.find_var(name.to_string());
                statement.compile(asm, prog)?;
                prog.pop(asm, "rax".to_string())?;
                asm.write(
                    format!(
                        "    mov     QWORD [rsp+{}], rax\n",
                        (prog.stack_pos - offset) * 8
                    )
                    .as_bytes(),
                )
            }
            Statement::Scope(statements) => {
                prog.enter_scope();
                for statement in statements {
                    statement.compile(asm, prog)?;
                }
                prog.leave_scope(asm)
            }
            Statement::ControlFlow(cf) => cf.compile(asm, prog),
            Statement::Dbg(arg) => {
                let _ = asm.write(";; DBG\n".as_bytes())?;
                arg.compile(asm, prog)?;
                prog.pop(asm, "rdi".to_string())?;
                asm.write("    call    dbg\n".as_bytes())
            }
            Statement::DereferenceActu(size, address, value) => {
                let _ = asm.write(";; Dereference(STATEMENT)\n".as_bytes())?;
                address.compile(asm, prog)?;
                value.compile(asm, prog)?;
                prog.pop(asm, "rbx".to_string())?;
                prog.pop(asm, "rax".to_string())?;
                match size {
                    64 => asm.write("    mov     QWORD [rax], rbx\n".as_bytes()),
                    32 => asm.write("    mov     DWORD [rax], ebx\n".as_bytes()),
                    16 => asm.write("    mov     WORD [rax], bx\n".as_bytes()),
                    8 => asm.write("    mov     BYTE [rax], bl\n".as_bytes()),
                    _ => unreachable!(),
                }
            }
            Statement::Drop(arg) => {
                arg.compile(asm, prog)?;
                let _ = asm.write(";; DROP\n".as_bytes())?;
                prog.pop(asm, "rax".to_string())
            }
        }
    }
}

impl Program {
    pub fn parse(tokens: &mut Vec<Token>) -> Self {
        let mut statements: Vec<Statement> = vec![];
        while let Some(value) = Statement::parse(tokens) {
            statements.push(value);
        }
        Self {
            statements,
            stack_pos: 0,
            vars: vec![],
            cf_id: 0,
            str_literals: HashMap::new(),
            str_literals_size: 0,
        }
    }

    fn push(&mut self, asm: &mut dyn Write, src: String) -> Result<usize, Error> {
        self.stack_pos += 1;
        asm.write(format!("    push    {src}\n").as_bytes())
    }

    fn pop(&mut self, asm: &mut dyn Write, dst: String) -> Result<usize, Error> {
        self.stack_pos -= 1;
        asm.write(format!("    pop     {dst}\n").as_bytes())
    }

    pub fn enter_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    pub fn leave_scope(&mut self, asm: &mut dyn Write) -> Result<usize, Error> {
        for _ in 0..self.vars.last().unwrap().len() {
            self.pop(asm, "rax".to_string())?;
        }
        self.vars.pop();
        Ok(0)
    }

    pub fn create_var(&mut self, name: String) {
        let local_vars = self.vars.last_mut().unwrap();
        if let hash_map::Entry::Vacant(e) = local_vars.entry(name.clone()) {
            e.insert(self.stack_pos + 1);
        } else {
            eprintln!("ERROR: Variable {name} is already define in the current scope");
            exit(1);
        }
    }

    pub fn find_var(&mut self, name: String) -> usize {
        for i in (0..self.vars.len()).rev() {
            let scope_vars = self.vars.get(i).unwrap();
            if let Some(offset) = scope_vars.get(&name) {
                return *offset;
            }
        }
        eprintln!("ERROR: Use of undeclared variable {name}");
        exit(1);
    }

    pub fn compile(&mut self, asm: &mut dyn Write) -> Result<usize, Error> {
        let _ = asm.write(
            "global _start

section .text

dbg:
    mov     r8, -3689348814741910323
    sub     rsp, 40
    mov     BYTE [rsp+31], 10
    lea     rcx, [rsp+30]
.L2:
    mov     rax, rdi
    mul     r8
    mov     rax, rdi
    shr     rdx, 3
    lea     rsi, [rdx+rdx*4]
    add     rsi, rsi
    sub     rax, rsi
    mov     rsi, rcx
    sub     rcx, 1
    add     eax, 48
    mov     BYTE [rcx+1], al
    mov     rax, rdi
    mov     rdi, rdx
    cmp     rax, 9
    ja      .L2
    lea     rdx, [rsp+32]
    mov     rdi, 1
    xor     eax, eax
    sub     rdx, rsi
    mov     rax, 1
    syscall
    add     rsp, 40
    ret

_start:
"
            .as_bytes(),
        )?; // ASM HEADER
        self.enter_scope();
        for statement in self.clone().statements {
            statement.compile(asm, self)?;
        }
        let _ = asm.write("    mov     rax, 60\n".as_bytes())?;
        let _ = asm.write("    mov     rdi, 0\n".as_bytes())?;
        let _ = asm.write("    syscall\n\n".as_bytes())?;
        let _ = asm.write("section .data\n".as_bytes())?;
        for (str_lit, idx) in self.str_literals.iter() {
            let _ = asm.write(format!("    STR_{idx} db ").as_bytes())?;
            for byte in str_lit.as_bytes() {
                let _ = asm.write(format!("{byte}, ").as_bytes())?;
            }
            let _ = asm.write("0\n".as_bytes())?;
        }
        Ok(0)
    }
}
//#endregion
