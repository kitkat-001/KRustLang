//! The module for compiling source code into byte code.

pub mod const_pool;

use crate::{log, lexer, parser};
use log::{Log, LogType, ErrorType, is_error};
use lexer::{TokenType, Token};
use parser::{Expression, ParserOutput};
use const_pool::ConstantPool;

use num_derive::FromPrimitive;

/// The OpCode used in the bytecode.
#[derive(FromPrimitive)]
pub enum OpCode
{
    Const,

    Push,
    PopInt,

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
    XorInt,
    OrInt
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
        let mut const_pool: ConstantPool = ConstantPool { pool: Vec::new(), ptr_size: cli_args[0] };
        let result: Result<Vec<u8>, ()> = generate_bytecode(parser_output.expr, &mut const_pool);
        if let Err(_) = result
        {
            logs.push(Log{log_type: LogType::Error(ErrorType::ExcessiveBytecode), line_and_col: None})
        }
        else if u32::from(cli_args[0]) * 8 < usize::BITS && byte_list.len() >= 1 << (cli_args[0] * 8)
        {
            logs.push(Log{log_type: LogType::Error(ErrorType::ExcessiveBytecode), line_and_col: None});
        }
        else
        {
            if const_pool.pool.len() != 0
            {
                byte_list.push(OpCode::Const as u8);
                byte_list.append(&mut usize_to_ptr_size(const_pool.pool.len(), const_pool.ptr_size));
                byte_list.append(&mut const_pool.pool);
            }
            let mut bytecode_core: Vec<u8> = result.ok().expect("if statement checked err");
            byte_list.append(&mut bytecode_core);
            byte_list.push(OpCode::PopInt as u8);
            bytecode = Some(byte_list);
        }
    }

    CompilerOutput { file_text: parser_output.file_text, bytecode, logs }
}

fn generate_bytecode(expr: Expression, const_pool: &mut ConstantPool) -> Result<Vec<u8>, ()>
{
    let mut bytecode: Vec<u8> = Vec::new();
    match expr
    {
        Expression::Binary { left, op, right , ..} =>
        {
            handle_binary(&mut bytecode, const_pool, left, op, right)?
        }
        Expression::Grouping { expr: child, .. } =>
        {
            bytecode.append(&mut generate_bytecode(*child, const_pool)?);
        }
        Expression::Literal { token , ..} =>
        {
            handle_literal(&mut bytecode, token, const_pool)?;
        }
        Expression::Unary { op, expr: child, .. } =>
        {
            bytecode.append(&mut generate_bytecode(*child, const_pool)?);
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
    Ok(bytecode)
}

// Handles binary expressions.
fn handle_binary(bytecode: &mut Vec<u8>, const_pool: &mut ConstantPool, left: Box<Expression>, op: Token, right: Box<Expression>) -> Result<(), ()>
{
    bytecode.append(&mut generate_bytecode(*left, const_pool)?);
    bytecode.append(&mut generate_bytecode(*right, const_pool)?);
    let ptr_size: u8 = const_pool.ptr_size;
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
        
        TokenType::Ampersand => { bytecode.push(OpCode::AndInt as u8); },
        TokenType::Caret => { bytecode.push(OpCode::XorInt as u8); },
        TokenType::Bar => { bytecode.push(OpCode::OrInt as u8); },
        _ => { panic!("invalid token found at head of binary expression.")}
    }
    Ok(())
}

// Handles literal expressions/tokens.
fn handle_literal(bytecode: &mut Vec<u8>, token: Token, const_pool: &mut ConstantPool) -> Result<(), ()>
{
    if let TokenType::IntLiteral(value) = token.token_type
    {
        bytecode.push(OpCode::Push as u8);
        bytecode.append(&mut const_pool.insert_int(value as i32)?.to_le_bytes()[0..const_pool.ptr_size as usize].to_vec());
        Ok(())
    }
    else 
    {
        panic!("all literals should have been accounted for");
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