//! The module for debug messages.

use std::fmt::{Display, Formatter, Result};

/// An enum representing anything that can be logged.
#[derive(Clone, PartialEq, Eq)]
pub enum LogType
{
    Warning(WarningType),
    Error(ErrorType)
}

#[derive(Clone, PartialEq, Eq)]
pub enum WarningType
{
    CLIArgRoundedDownU16(String, u16),
    CLITargetLargerThanMachine(usize),
}

/// An enum representing any possible error.
#[derive(Clone, PartialEq, Eq)]
pub enum ErrorType
{
    FatalError,

    CLIMultipleFiles,
    CLICantReadArgs,
    CLINoArgs,
    CLIRequiresArg(String),
    CLIRequiresNumArg(String),
    CLIRequiresNumArgLessThanU16(String, u16),
    CLIRequiresNumArgAtLeastU16(String, u16),
    CLIUnrecognizedArg(String),
    CLICantOpenFile(String),
    CLINoFile,
    CLIFileToBig(usize),

    UnrecognizedToken(String),
    UnrepresentableIntegerLiteral(String),
    
    ExpectedEOF,
    UnexpectedEOF,
    UnexpectedToken,
    ExpectedExpressionInParens,
    ExpectedCloseParen,
    UnnegatedMinimumIntegerLiteral,

    ExcessiveBytecode,

    CompiledForDifferentTarget(usize),
    DivideByZero,
}

/// Represents all possible errors as well as helpful debug information when relevant.
#[derive(Clone, PartialEq, Eq)]
pub struct Log
{
    pub log_type: LogType,
    pub line_and_col: Option<(usize, usize)>
}

impl Display for Log
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result
    {
        if let LogType::Error(error_type) = self.log_type.clone()
        {
            if let ErrorType::FatalError = error_type
            {
                return write!(f, "fatal error; program terminated");
            }
        }

        let log_type: String = match self.log_type.clone() {
            LogType::Warning(_) => "warning".to_string(),
            LogType::Error(_) => "error".to_string(),
        };

        let message: String = { match self.log_type.clone() {
            LogType::Warning(warning_type) => {match warning_type
            {
                WarningType::CLIArgRoundedDownU16(arg, value)
                    => format!("argument of \"{arg}\" will be rounded down to the nearest multiple of {value}."),
                WarningType::CLITargetLargerThanMachine(ptr_size)
                    => format!("warning: this program is being compiled for a {ptr_size}-bit machine, while this is only a {}-bit machine.", 
                    usize::BITS)
            }},
            LogType::Error(error_type) => {match error_type
            {
                ErrorType::FatalError => String::new(), // dealt with above

                ErrorType::CLIMultipleFiles => "command line contains multiple files.".to_string(),
                ErrorType::CLICantReadArgs => "could not read command line arguments.".to_string(),
                ErrorType::CLINoArgs => "no command line arguments.".to_string(),
                ErrorType::CLIRequiresArg(arg) 
                    => format!("compiler flag \"{arg}\" requires an argument."),
                ErrorType::CLIRequiresNumArg(arg) 
                    => format!("compiler flag \"{arg}\" requires a numerical argument."),
                ErrorType::CLIRequiresNumArgLessThanU16(arg, bound) 
                    => format!("compiler flag \"{arg}\" requires an argument less than {bound}."),
                ErrorType::CLIRequiresNumArgAtLeastU16(arg, bound) 
                    => format!("compiler flag \"{arg}\" requires an argument that's at least {bound}."),
                ErrorType::CLIUnrecognizedArg(arg)
                    => format!("unrecognized argument \"{arg}\"."),
                ErrorType::CLICantOpenFile(path)
                    => format!("could not open file \"{path}\"."),
                ErrorType::CLINoFile => "no source file entered.".to_string(),
                ErrorType::CLIFileToBig(ptr_size) 
                    => format!("the file is too big to compile for a {ptr_size}-bit machine."),

                ErrorType::UnrecognizedToken(token) 
                    => format!("unrecognized token \"{token}\"."),
                ErrorType::UnrepresentableIntegerLiteral(token) 
                    => format!("int literal \"{token}\" must be at most {}.", 0x_8000_0000u32),

                ErrorType::ExpectedEOF => "expected end of file.".to_string(),
                ErrorType::UnexpectedEOF => "unexpected end of file.".to_string(),
                ErrorType::UnexpectedToken => "unexpected token.".to_string(),
                ErrorType::ExpectedExpressionInParens => "expected expression within parentheses.".to_string(),
                ErrorType::ExpectedCloseParen => "expected \')\' following \'(\'.".to_string(),
                ErrorType::UnnegatedMinimumIntegerLiteral
                    => format!("the int literal {} must be preceded by a unary \'-\' operator.", 0x8000_0000u32),

                ErrorType::ExcessiveBytecode => "could not compile as bytecode was too large.".to_string(),
            
                
                ErrorType::CompiledForDifferentTarget(ptr_size) 
                    => format!("this program was compiled for a {ptr_size}-bit machine, while this is only a {}-bit machine.", usize::BITS),
                ErrorType::DivideByZero => "division by zero.".to_string(),
            }},
        }};
        if let None = self.line_and_col
        {
            write!(f, "{log_type}: {message}")
        }
        else 
        {
            write!(f, "{log_type} (line {}:{}): {message}", 
                self.line_and_col.expect("checked by if statement").0, 
                self.line_and_col.expect("checked by if statement").1)    
        }
    }
}

/// Returns whether or not a list of logs contains an error.
pub fn is_error(logs: &Vec<Log>) -> bool
{
    for log in logs
    {
        if let LogType::Error(_) = log.log_type
        {
            return true;
        }
    }
    false
}

/// Converts all logs into strings.
pub fn all_to_string(logs: &Vec<Log>) -> Vec<String>
{
    let mut strings: Vec<String> = Vec::new();
    for log in logs
    {
        strings.push(format!("{log}"));
    }
    return strings;
}