//! The module for reading from the command line.

use crate::log;
use std::cmp::min;
use std::env::args;
use std::fs::read_to_string;
use std::io;
use std::num::ParseIntError;
use std::panic::catch_unwind;
use std::str::ParseBoolError;
use std::thread;
use log::{Log, LogType, ErrorType, WarningType};

/// The result from reading the command line without errors.
pub struct CLIInfo
{
    pub file_path: String,
    pub cli_args: [u8; 2],
}

const COMPILER_FLAGS: [&str; 2] = ["-pointer_size", "-detailed_errors"];

/// Get file name and compiler flags from the command line.
pub fn read_command_line() -> (Option<CLIInfo>, Vec<Log>)
{
    let input: Result<Vec<String>, Vec<Log>>  = get_args();
    if input.is_err() {return (None, input.err().expect("checked by if statement"))};
    let input: Vec<String> = input.ok().expect("checked by if statement");
    let mut file_path: Option<String> = None;
    let mut ptr_size: u16 = min(usize::BITS, 2047).try_into().expect("should be valid as max value is less than u16::MAX");
    let mut detailed_err: bool = true;
    let mut logs: Vec<Log> = Vec::new();
    let mut multiple_file_error: bool = false;
    for arg in input
    {
        if arg.ends_with(".txt") && multiple_file_error == false
        {
            if let None = file_path
            {
                file_path = Some(arg.to_string());
            }
            else
            {
                logs.push(Log{log_type: LogType::Error(ErrorType::CLIMultipleFiles), line_and_col: None});
                file_path = None;
                multiple_file_error = true;
            }
        }
        else if arg.starts_with(COMPILER_FLAGS[0])
        {
            ptr_size = handle_ptr_size(&arg, &mut logs, ptr_size);
        }
        else if arg.starts_with(COMPILER_FLAGS[1])
        {
            detailed_err = handle_detailed_err(&arg, &mut logs);
        }
        else
        {
            handle_unrecognized_flag(&arg, &mut logs);
        }
    }
    
    get_result(file_path, &mut logs, ptr_size, detailed_err, multiple_file_error)
}

// Get the arguments from the command line.
fn get_args<'a>() -> Result<Vec<String>, Vec<Log>>
{
    let input: thread::Result<Vec<String>> = catch_unwind(||{args().collect()});
    if input.is_err()
    {
        return Err(vec!(Log{log_type: LogType::Error(ErrorType::CLICantReadArgs), line_and_col: None}));
    }
    let mut input: Vec<String> = input.ok().expect("should be valid as error handled earlier");
    if input.len() == 1
    {
        return Err(vec!(Log{log_type: LogType::Error(ErrorType::CLINoArgs), line_and_col: None}));
    }
    input.remove(0);
    Ok(input)
}

// Handle the pointer size compiler flag.
fn handle_ptr_size (arg: &String, logs: &mut Vec<Log>, mut ptr_size: u16) -> u16
{
    let arg: &str = &arg[COMPILER_FLAGS[0].len()..];
    if !arg.starts_with("=")
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::CLIRequiresArg(COMPILER_FLAGS[0].to_string())), line_and_col: None});
    }
    else 
    {
        let parsed_arg: Result<u16, ParseIntError> = arg[1..].parse::<u16>();
        if let Err(ParseIntError{..}) = parsed_arg
        {
            logs.push(Log{log_type: LogType::Error(ErrorType::CLIRequiresNumArg(COMPILER_FLAGS[0].to_string())), line_and_col: None});
        }
        else 
        {
            ptr_size = parsed_arg.ok().expect("should be valid as error handled earlier.");
        }
        if ptr_size >= 2048
        {
            logs.push(Log{log_type: LogType::Error(ErrorType::CLIRequiresNumArgLessThanU16(COMPILER_FLAGS[0].to_string(), 2048)), line_and_col: None});
        }
        if ptr_size < 8
        {
            logs.push(Log{log_type: LogType::Error(ErrorType::CLIRequiresNumArgAtLeastU16(COMPILER_FLAGS[0].to_string(), 8)), line_and_col: None});
        }
    }
    ptr_size
}

// Handle the detail compiler flag.
fn handle_detailed_err (arg: &String, logs: &mut Vec<Log>) -> bool
{
    let arg: &str = &arg[COMPILER_FLAGS[1].len()..];
    if !arg.starts_with("=")
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::CLIRequiresArg(COMPILER_FLAGS[1].to_string())), line_and_col: None});
    }
    else 
    {
        let parsed_arg: Result<bool, ParseBoolError> = arg[1..].parse::<bool>();
        if let Err(ParseBoolError{..}) = parsed_arg
        {
            logs.push(Log{log_type: LogType::Error(ErrorType::CLIRequiresBoolArg(COMPILER_FLAGS[1].to_string())), line_and_col: None});
        }
        else 
        {
            return parsed_arg.ok().expect("should be valid as error handled earlier.");
        }
    }
    true
}

// Handle unrecognized flags in the command line.
fn handle_unrecognized_flag(arg: &String, logs: &mut Vec<Log>)
{
    let index: Option<usize> = arg.find('=');
    let mut arg_substr: &str = &arg[..];
    if let Some(i) = index
    {
        arg_substr = &arg[..i]
    }
    logs.push(Log{log_type: LogType::Error(ErrorType::CLIUnrecognizedArg(arg_substr.to_string())), line_and_col: None});
}

// Gets the CLI info.
fn get_result(
    file_path: Option<String>, 
    logs: &mut Vec<Log>, 
    ptr_size: u16, 
    detailed_err: bool,
    multiple_file_error: bool)
    -> (Option<CLIInfo>, Vec<Log>)
{
    let file_size: usize = get_file_size(&file_path, logs, multiple_file_error);

    if logs.len() > 0
    {
        (None, logs.clone())
    }
    else 
    {
        handle_compiler_flag_issues(&file_path, logs, ptr_size, detailed_err, file_size)
    }
}

// Get the size of the file, if exactly one is given.
fn get_file_size(file_path: &Option<String>, logs: &mut Vec<Log>, multiple_file_error: bool) -> usize
{
    let mut file_size: usize = 0;
    if let Some(ref path) = file_path
    {
        let result: io::Result<String> = read_to_string(path);
        if result.is_err()
        {
            logs.push(Log{log_type: LogType::Error(ErrorType::CLICantOpenFile(path.clone())), line_and_col: None});
        }
        else 
        {
            file_size = result.ok().expect("should be valid as error handled above").len();    
        }
    }
    else if !multiple_file_error
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::CLINoFile), line_and_col: None});
    }
    file_size
}

// Deal with issues relating to compiler flag values.
fn handle_compiler_flag_issues(
    file_path: &Option<String>, 
    logs: &mut Vec<Log>, 
    ptr_size: u16, 
    detailed_err: bool,
    file_size: usize) 
    -> (Option<CLIInfo>, Vec<Log>)
{
    if let Some(file_path) = file_path
    {
        let detailed_err: u8 = if detailed_err {1} else {0};
        let ptr_size_bytes: u8 = (ptr_size / 8).try_into().expect("ptr_size maximum is less than 2048");
        if ptr_size % 8 != 0
        {
            logs.push(Log{log_type: LogType::Warning(WarningType::CLIArgRoundedDownU16(COMPILER_FLAGS[0].to_string(), 8)), 
                line_and_col: None});
        }
        let ptr_size: usize = <u8 as Into<usize>>::into(ptr_size_bytes) * 8;
        if ptr_size > usize::BITS.try_into().expect("max value of usize must be less than the number of bits")
        {
            logs.push(Log{log_type: LogType::Warning(WarningType::CLITargetLargerThanMachine(ptr_size)), 
                line_and_col: None});
        }
        else if ptr_size < usize::BITS.try_into().expect("max value of usize must be less than the number of bits")
            && file_size > 1 << ptr_size
        {
            logs.push(Log{log_type: LogType::Error(ErrorType::CLIFileToBig(ptr_size)),
                line_and_col: None});
            return (Some(CLIInfo{ file_path: file_path.to_string(), cli_args: [ptr_size_bytes, detailed_err] }), logs.clone());
        }
        (Some(CLIInfo{ file_path: file_path.to_string(), cli_args: [ptr_size_bytes, detailed_err] }), logs.clone())
    }
    else 
    {
        panic!("should never reach here since error list empty.")
    }
}