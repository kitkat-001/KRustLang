//! The module for compiling source code into byte code.

use crate::{log, lexer, parser};
use log::{Log, LogType, ErrorType, is_error};
use lexer::{TokenType, Token};
use parser::{Expression, ParserOutput, Type};

use num_derive::FromPrimitive;

/// The OpCode used in the bytecode.
#[derive(FromPrimitive)]
pub enum OpCode
{
    PushInt,
    PushByte,
    PopInt,
    PopBool,

    MinusInt,

    AddInt,
    SubtractInt,
    MultiplyInt,
    DivideInt,
    ModuloInt,

    ComplementInt,

    LeftShiftInt,
    RightShiftInt,

    AndInt,
    AndBool,
    XorInt,
    XorBool,
    OrInt,
    OrByte
}

/// The output given by the compiler.
pub struct CompilerOutput
{
    pub file_text: String,
    pub bytecode: Option<Vec<u8>>,
    pub logs: Vec<Log>,
}

/// Compiles to bytecode.
pub fn compile(parser_output: ParserOutput, cli_args: [u8; 2]) -> CompilerOutput
{
    let mut bytecode: Option<Vec<u8>> = None;
    let mut logs: Vec<Log> = parser_output.logs.clone();

    if !is_error(&logs)
    {
        let mut byte_list: Vec<u8> = cli_args.to_vec();
        let expr_type: Type = parser_output.expr.get_type().expect("any \"None\" should have a parsing error");
        byte_list.append(&mut generate_bytecode(parser_output.expr, cli_args[0]));
        byte_list.push(match expr_type {
            Type::Int => OpCode::PopInt,
            Type::Bool => OpCode::PopBool,
        } as u8);
        if u32::from(cli_args[0]) * 8 < usize::BITS && byte_list.len() >= 1 << (cli_args[0] * 8)
        {
            logs.push(Log{log_type: LogType::Error(ErrorType::ExcessiveBytecode), line_and_col: None});
        }
        else
        {
            bytecode = Some(byte_list);
        }
    }

    CompilerOutput { file_text: parser_output.file_text, bytecode, logs }
}

fn generate_bytecode(expr: Expression, ptr_size: u8) -> Vec<u8>
{
    let mut bytecode: Vec<u8> = Vec::new();
    match expr
    {
        Expression::Binary { left, op, right, expr_type} =>
        {
            handle_binary(&mut bytecode, ptr_size, left, op, right, expr_type.expect("any \"None\" should have a parsing error"));
        }
        Expression::Grouping { expr: child, .. } =>
        {
            bytecode.append(&mut generate_bytecode(*child, ptr_size));
        }
        Expression::Literal { token , ..} =>
        {
            handle_literal(&mut bytecode, token);
        }
        Expression::Unary { op, expr: child, .. } =>
        {
            bytecode.append(&mut generate_bytecode(*child, ptr_size));
            if op.token_type == TokenType::Minus
            {
                bytecode.push(OpCode::MinusInt as u8);
            }
            if op.token_type == TokenType::Tilde
            {
                bytecode.push(OpCode::ComplementInt as u8);
            }
        }
        _ => panic!("all expression types should have been accounted for"),
    }
    bytecode
}

// Handles binary expressions.
fn handle_binary(bytecode: &mut Vec<u8>, ptr_size: u8, left: Box<Expression>, op: Token, right: Box<Expression>, expr_type: Type)
{
    bytecode.append(&mut generate_bytecode(*left, ptr_size));
    bytecode.append(&mut generate_bytecode(*right, ptr_size));
    match op.token_type
    {
        TokenType::Plus => { bytecode.push(OpCode::AddInt as u8); },
        TokenType::Minus => { bytecode.push(OpCode::SubtractInt as u8); },
        TokenType::Star => { bytecode.push(OpCode::MultiplyInt as u8); },
        TokenType::Slash => 
        {
                bytecode.push(OpCode::DivideInt as u8); 
                bytecode.append(&mut usize_to_ptr_size(op.line, ptr_size));
                bytecode.append(&mut usize_to_ptr_size(op.col, ptr_size));
        },
        TokenType::Percent => 
        {
                bytecode.push(OpCode::ModuloInt as u8); 
                bytecode.append(&mut usize_to_ptr_size(op.line, ptr_size));
                bytecode.append(&mut usize_to_ptr_size(op.col, ptr_size));
        },

        TokenType::LeftShift => { bytecode.push(OpCode::LeftShiftInt as u8); },
        TokenType::RightShift => { bytecode.push(OpCode::RightShiftInt as u8); },
        
        TokenType::Ampersand => { 
            bytecode.push( match expr_type
                {
                    Type::Int => OpCode::AndInt,
                    Type::Bool => OpCode::AndBool,
                } as u8); 
        },
        TokenType::Caret => {
            bytecode.push( match expr_type
                {
                    Type::Int => OpCode::XorInt,
                    Type::Bool => OpCode::XorBool,
                } as u8); 
        },
        TokenType::Bar => {
            bytecode.push( match expr_type
                {
                    Type::Int => OpCode::OrInt,
                    Type::Bool => OpCode::OrByte,
                } as u8); 
        },
        _ => { panic!("invalid token found at head of binary expression.")}
    }
}

// Handles literal expressions/tokens.
fn handle_literal(bytecode: &mut Vec<u8>, token: Token)
{
    match token.token_type {
        TokenType::IntLiteral(value) =>
        {
            bytecode.push(OpCode::PushInt as u8);
            bytecode.append(&mut value.to_le_bytes().to_vec());
        }
        TokenType::True =>
        {
            bytecode.push(OpCode::PushByte as u8);
            bytecode.push(1 as u8);
        }
        TokenType::False =>
        {
            bytecode.push(OpCode::PushByte as u8);
            bytecode.push(0 as u8);
        }
        _ => panic!("all literals should have been accounted for")
    }
}

// Converts a usize value to a list of bytes with a length of ptr_size.
fn usize_to_ptr_size(value: usize, ptr_size: u8) -> Vec<u8>
{
    let usize_size_bytes: u32 = usize::BITS / 8;
    let bytes: [u8; 8] = value.to_le_bytes();
    let bytes: &[u8] = &bytes[..u32::min(ptr_size as u32, usize_size_bytes) as usize];
    let mut bytes: Vec<u8> = bytes.to_vec();
    while bytes.len() < ptr_size as usize
    {
        bytes.push(0);
    } 
    bytes
}