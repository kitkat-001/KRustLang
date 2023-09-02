//! The module for error handling.

use std::fmt::{Display, Formatter, Result};

/// An enum representing any possible compile time error.
#[derive(Clone)]
pub enum ErrorType
{
    FatalError,

    UnrecognizedToken(String),
    UnrepresentableIntegerLiteral(String),
    
    ExpectedEOF,
    UnexpectedEOF,
    UnexpectedToken,
    ExpectedExpressionInParens,
    ExpectedCloseParen,
    UnnegatedMinimumIntegerLiteral,

    ExcessiveBytecode,

    DivideByZero,
}

/// Represents all possible errors as well as helpful debug information when relevant.
pub struct Error
{
    pub error_type: ErrorType,
    pub line_and_col: Option<(usize, usize)>
}

impl Display for Error
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result
    {
        if let ErrorType::FatalError = self.error_type
        {
            return write!(f, "fatal error; program terminated");
        }

        let message: String = { match self.error_type.clone()
        {
            ErrorType::FatalError => "".to_string(), // dealt with above

            ErrorType::UnrecognizedToken(token) 
                => format!("unrecognized token \"{}\"", token),
            ErrorType::UnrepresentableIntegerLiteral(token) 
                => format!("int literal \"{}\" must be at most {}", token, 0x_8000_0000u32),

            ErrorType::ExpectedEOF => "expected end of file".to_string(),
            ErrorType::UnexpectedEOF => "unexpected end of file".to_string(),
            ErrorType::UnexpectedToken => "unexpected token".to_string(),
            ErrorType::ExpectedExpressionInParens => "expected expression within parentheses".to_string(),
            ErrorType::ExpectedCloseParen => "expected \')\' following \'(\'".to_string(),
            ErrorType::UnnegatedMinimumIntegerLiteral
                => format!("the int literal {} must be preceded by a unary \'-\' operator", 0x8000_0000u32),

            ErrorType::ExcessiveBytecode => "could not compile as bytecode was too large".to_string(),
        
            ErrorType::DivideByZero => "division by zero".to_string(),
        }};
        if let None = self.line_and_col
        {
            write!(f, "error: {message}")
        }
        else 
        {
            write!(f, "error (line {}:{}): {message}", 
                self.line_and_col.expect("checked by if statement").0, 
                self.line_and_col.expect("checked by if statement").1)    
        }
    }
}