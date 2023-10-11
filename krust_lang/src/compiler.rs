//! The module for compiling source code into byte code.

use crate::{lexer, parser, util::log};
use lexer::{Token, TokenType};
use log::{is_error, ErrorType, Log, LogType};
use parser::{Expression, ParserOutput, Type};

use num_derive::FromPrimitive;

/// The number of bytes used to keep track of variables.
pub const BYTES_PER_VAR: usize = 2;

/// The `OpCode` used in the bytecode.
#[derive(FromPrimitive, Clone, Copy)]
pub enum OpCode {
    // Stack operators
    PushInt,
    PushByte,
    PopInt,
    PopByte,
    PrintInt,
    PrintBool,

    // Variable operators
    AllocInt,
    AllocBool,
    GetInt,
    GetBool,
    SetInt,
    SetBool,

    // Casting/conversion operators
    IntToBool,
    BoolToInt,

    // Arithmetic operators
    MinusInt,
    AddInt,
    SubtractInt,
    MultiplyInt,
    DivideInt,
    ModuloInt,

    // Comparison operators.
    LessInt,
    LessEqualInt,
    GreaterInt,
    GreaterEqualInt,

    // Boolean operators
    Not,

    // Bitwise operators
    ComplementInt,
    AndInt,
    AndByte,
    XorInt,
    XorByte,
    OrInt,
    OrByte,

    // Shifts
    LeftShiftInt,
    RightShiftInt,

    // Equality operators
    EqualityInt,
    EqualityByte,
    InequalityInt,
    InequalityByte,
}

/// The output given by the compiler.
pub struct CompilerOutput {
    pub file_text: String,
    pub bytecode: Option<Vec<u8>>,
    pub logs: Vec<Log>,
}

/// Compiles to bytecode.
#[must_use]
#[allow(clippy::missing_panics_doc)] // Should never actually panic.
pub fn compile(parser_output: ParserOutput, cli_args: [u8; 2]) -> CompilerOutput {
    let mut bytecode: Option<Vec<u8>> = None;
    let mut logs: Vec<Log> = parser_output.logs.clone();

    if !is_error(&logs) {
        let mut byte_list: Vec<u8> = cli_args.to_vec();
        let expr_type: Type = parser_output
            .expr
            .get_type()
            .expect("any \"None\" should have a parsing error");
        byte_list.append(&mut generate_bytecode(
            &parser_output.expr,
            cli_args[0],
            &mut logs,
            &mut Vec::new(),
        ));
        if expr_type != Type::Void {
            byte_list.push(match expr_type {
                Type::Int => OpCode::PrintInt,
                Type::Bool => OpCode::PrintBool,
                Type::Void => panic!("Should have been caught by above if statement."),
            } as u8);
        }
        if u32::from(cli_args[0]) * 8 < usize::BITS && byte_list.len() >= 1 << (cli_args[0] * 8) {
            logs.push(Log {
                log_type: LogType::Error(ErrorType::ExcessiveBytecode),
                line_and_col: None,
            });
        } else {
            bytecode = Some(byte_list);
        }
    }

    CompilerOutput {
        file_text: parser_output.file_text,
        bytecode,
        logs,
    }
}

fn generate_bytecode(
    expr: &Expression,
    ptr_size: u8,
    logs: &mut Vec<Log>,
    var_list: &mut Vec<Token>,
) -> Vec<u8> {
    let mut bytecode: Vec<u8> = Vec::new();
    match expr {
        Expression::Binary {
            left,
            op,
            right,
            expr_type,
        } => {
            handle_binary(
                &mut bytecode,
                ptr_size,
                left,
                *op,
                right,
                expr_type.expect("any \"None\" should have a parsing error"),
                logs,
                var_list,
            );
        }
        Expression::Cast { expr_type, expr } => {
            bytecode.append(&mut generate_bytecode(expr, ptr_size, logs, var_list));
            let cast_op: Option<OpCode> = match expr.get_type() {
                Some(Type::Int) => match expr_type {
                    Some(Type::Int) => None,
                    Some(Type::Bool) => Some(OpCode::IntToBool),
                    _ => panic!("no other types should be possible."),
                },
                Some(Type::Bool) => match expr_type {
                    Some(Type::Int) => Some(OpCode::BoolToInt),
                    Some(Type::Bool) => None,
                    _ => panic!("no other types should be possible."),
                },
                Some(Type::Void) => match expr_type {
                    Some(Type::Void) => None,
                    _ => panic!("no other types should be possible."),
                },
                None => panic!("should not be able to cast from a None type"),
            };
            if let Some(cast_op) = cast_op {
                bytecode.push(cast_op as u8);
            }
        }
        Expression::ExpressionList { list } => {
            for expr in list {
                bytecode.append(&mut generate_bytecode(expr, ptr_size, logs, var_list));
            }
        }
        Expression::Grouping { expr: child, .. } => {
            bytecode.append(&mut generate_bytecode(child, ptr_size, logs, var_list));
        }
        Expression::Literal { token, .. } => {
            handle_literal(&mut bytecode, *token);
        }
        Expression::Statement { expr } => {
            bytecode.append(&mut generate_bytecode(expr, ptr_size, logs, var_list));
            if expr.get_type() != Some(Type::Void) {
                bytecode.push(match expr.get_type() {
                    Some(Type::Int) => OpCode::PopInt,
                    Some(Type::Bool) => OpCode::PopByte,
                    _ => panic!("This type is invalid."),
                } as u8);
            }
        }
        Expression::Unary {
            op, expr: child, ..
        } => {
            bytecode.append(&mut generate_bytecode(child, ptr_size, logs, var_list));
            bytecode.push(match op.token_type {
                TokenType::Minus => OpCode::MinusInt,
                TokenType::Tilde => OpCode::ComplementInt,
                TokenType::ExclamationMark => OpCode::Not,
                _ => panic!("all unary operators should have been accounted for"),
            } as u8);
        }
        Expression::Variable {
            initialized,
            token,
            expr_type,
        } => {
            // This handles get expressions; set expressions handled with other binary expressions.
            if *initialized {
                let index: Option<usize> = var_list.iter().position(|t| t == token);
                assert!(index.is_some(), "variable should be in var_list");
                let index: usize = index.expect("checked by if");
                bytecode.push(match expr_type {
                    Some(Type::Int) => OpCode::GetInt,
                    Some(Type::Bool) => OpCode::GetBool,
                    _ => panic!("all variable types should have been accounted for",),
                } as u8);
                bytecode.append(&mut index.to_le_bytes()[0..BYTES_PER_VAR].to_vec());
            }
        }
        Expression::VariableDeclaration { initialized_var } => {
            if let Expression::Variable {
                token, expr_type, ..
            } = **initialized_var
            {
                if var_list.len() == 1 << (8 * BYTES_PER_VAR) {
                    // Equals rather than greater or equals so that this only happens once.
                    logs.push(Log {
                        log_type: LogType::Error(ErrorType::TooManyVariables(BYTES_PER_VAR)),
                        line_and_col: None, // TODO: Should this contain line and col of declaration of variable that pushes compiler past the limit?
                    });
                }
                var_list.push(token);
                bytecode.push(match expr_type {
                    Some(Type::Int) => OpCode::AllocInt,
                    Some(Type::Bool) => OpCode::AllocBool,
                    _ => panic!("all variable types should have been accounted for",),
                } as u8);
            } else {
                panic!("variable declarations should always contain variables.")
            }
        }
        // Void expressions are empty; cast and type expressions shouldn't occur in isolation.
        Expression::CastOp { .. } | Expression::Type { .. } | Expression::Void => {}
        Expression::EOF | Expression::Null => {
            panic!("all expression types should have been accounted for")
        }
    }
    bytecode
}

// Handles binary expressions.
fn handle_binary(
    bytecode: &mut Vec<u8>,
    ptr_size: u8,
    left: &Expression,
    op: Token,
    right: &Expression,
    expr_type: Type,
    logs: &mut Vec<Log>,
    var_list: &mut Vec<Token>,
) {
    // Variable expressions are weird and need to be handled separately.
    if op.token_type != TokenType::Equals {
        bytecode.append(&mut generate_bytecode(left, ptr_size, logs, var_list));
        bytecode.append(&mut generate_bytecode(right, ptr_size, logs, var_list));
    }
    match op.token_type {
        TokenType::Equals => {
            let mut var: Expression = left.clone();
            if let Expression::VariableDeclaration { initialized_var } = var {
                // This should only run if this is a declaration, not a lone variable; otherwise this could be interpreted as a get.
                bytecode.append(&mut generate_bytecode(left, ptr_size, logs, var_list));
                var = *initialized_var;
            }
            bytecode.push(match expr_type {
                Type::Int => OpCode::PopInt,
                Type::Bool => OpCode::PopByte,
                Type::Void => panic!("all variable types should have been accounted for",),
            } as u8);
            bytecode.append(&mut generate_bytecode(right, ptr_size, logs, var_list));
            if let Expression::Variable {
                initialized,
                token,
                expr_type,
            } = var
            {
                if initialized {
                    let index: Option<usize> = var_list.iter().position(|t| *t == token);
                    assert!(index.is_some(), "variable should be in var_list");
                    let index: usize = index.expect("checked by if");
                    bytecode.push(match expr_type {
                        Some(Type::Int) => OpCode::SetInt,
                        Some(Type::Bool) => OpCode::SetBool,
                        _ => panic!("all variable types should have been accounted for",),
                    } as u8);
                    bytecode.append(&mut index.to_le_bytes()[0..BYTES_PER_VAR].to_vec());
                }
            }
        }

        TokenType::Plus => {
            bytecode.push(OpCode::AddInt as u8);
        }
        TokenType::Minus => {
            bytecode.push(OpCode::SubtractInt as u8);
        }
        TokenType::Star => {
            bytecode.push(OpCode::MultiplyInt as u8);
        }
        TokenType::Slash => {
            bytecode.push(OpCode::DivideInt as u8);
            bytecode.append(&mut usize_to_ptr_size(op.line, ptr_size));
            bytecode.append(&mut usize_to_ptr_size(op.col, ptr_size));
        }
        TokenType::Percent => {
            bytecode.push(OpCode::ModuloInt as u8);
            bytecode.append(&mut usize_to_ptr_size(op.line, ptr_size));
            bytecode.append(&mut usize_to_ptr_size(op.col, ptr_size));
        }

        TokenType::Less => {
            bytecode.push(OpCode::LessInt as u8);
        }
        TokenType::LessEqual => {
            bytecode.push(OpCode::LessEqualInt as u8);
        }
        TokenType::Greater => {
            bytecode.push(OpCode::GreaterInt as u8);
        }
        TokenType::GreaterEqual => {
            bytecode.push(OpCode::GreaterEqualInt as u8);
        }

        TokenType::Ampersand => {
            bytecode.push(match expr_type {
                Type::Int => OpCode::AndInt,
                Type::Bool => OpCode::AndByte,
                Type::Void => panic!("Invalid type for this operation"),
            } as u8);
        }
        TokenType::Caret => {
            bytecode.push(match expr_type {
                Type::Int => OpCode::XorInt,
                Type::Bool => OpCode::XorByte,
                Type::Void => panic!("Invalid type for this operation"),
            } as u8);
        }
        TokenType::Bar => {
            bytecode.push(match expr_type {
                Type::Int => OpCode::OrInt,
                Type::Bool => OpCode::OrByte,
                Type::Void => panic!("Invalid type for this operation"),
            } as u8);
        }

        TokenType::LeftShift => {
            bytecode.push(OpCode::LeftShiftInt as u8);
        }
        TokenType::RightShift => {
            bytecode.push(OpCode::RightShiftInt as u8);
        }

        TokenType::Equality => {
            bytecode.push(match &left.get_type() {
                Some(Type::Int) => OpCode::EqualityInt,
                Some(Type::Bool) => OpCode::EqualityByte,
                _ => panic!("No other type should be possible."),
            } as u8);
        }
        TokenType::Inequality => {
            bytecode.push(match &left.get_type() {
                Some(Type::Int) => OpCode::InequalityInt,
                Some(Type::Bool) => OpCode::InequalityByte,
                _ => panic!("No other type should be possible."),
            } as u8);
        }

        _ => {
            panic!("invalid token found at head of binary expression.")
        }
    }
}

// Handles literal expressions/tokens.
fn handle_literal(bytecode: &mut Vec<u8>, token: Token) {
    match token.token_type {
        TokenType::IntLiteral(value) => {
            bytecode.push(OpCode::PushInt as u8);
            bytecode.append(&mut value.to_le_bytes().to_vec());
        }
        TokenType::True => {
            bytecode.push(OpCode::PushByte as u8);
            bytecode.push(1u8);
        }
        TokenType::False => {
            bytecode.push(OpCode::PushByte as u8);
            bytecode.push(0u8);
        }
        _ => panic!("all literals should have been accounted for"),
    }
}

// Converts a usize value to a list of bytes with a length of ptr_size.
fn usize_to_ptr_size(value: usize, ptr_size: u8) -> Vec<u8> {
    let usize_size_bytes: u32 = usize::BITS / 8;
    let bytes: [u8; 8] = value.to_le_bytes();
    let bytes: &[u8] = &bytes[..u32::min(u32::from(ptr_size), usize_size_bytes) as usize];
    let mut bytes: Vec<u8> = bytes.to_vec();
    while bytes.len() < ptr_size as usize {
        bytes.push(0);
    }
    bytes
}
