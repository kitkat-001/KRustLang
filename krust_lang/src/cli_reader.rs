//! The module for reading from the command line.

use std::cmp::min;
use std::env::args;
use std::fs::read_to_string;
use std::io;
use std::num::ParseIntError;
use std::panic::catch_unwind;
use std::thread;

/// The result from reading the command line without errors.
pub struct CLIInfo
{
    pub file_path: String,
    pub cli_args: [u8; 1],
}

const COMPILER_FLAGS: [&str; 1] = ["-pointer_size"];

/// Get file name and compiler flags from the command line.
pub fn read_command_line() -> Result<CLIInfo, Vec<String>>
{
    let input: Vec<String> = get_args()?;
    let mut file_path: Option<String> = None;
    let mut ptr_size: u16 = min(usize::BITS, 2047).try_into().expect("should be valid as max value is less than u16::MAX");
    let mut errors: Vec<String> = Vec::new();
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
                errors.push("error: command line contains multiple files.".to_string());
                file_path = None;
                multiple_file_error = true;
            }
        }
        else if arg.starts_with(COMPILER_FLAGS[0])
        {
            ptr_size = handle_ptr_size(&arg, &mut errors, ptr_size);
        }
        else
        {
            handle_unrecognized_flag(&arg, &mut errors);
        }
    }
    
    get_result(file_path, &mut errors, ptr_size, multiple_file_error)
}

// Get the arguments from the command line.
fn get_args<'a>() -> Result<Vec<String>, Vec<String>>
{
    let input: thread::Result<Vec<String>> = catch_unwind(||{args().collect()});
    if input.is_err()
    {
        return Err(vec!(String::from("error: could not read command line arguments")));
    }
    let mut input: Vec<String> = input.ok().expect("should be valid as error handled earlier");
    if input.len() == 1
    {
        return Err(vec!("error: no command line arguments".to_string()));
    }
    input.remove(0);
    Ok(input)
}

// Handle the pointer size compiler flag.
fn handle_ptr_size (arg: &String, errors: &mut Vec<String>, mut ptr_size: u16) -> u16
{
    let arg: &str = &arg[COMPILER_FLAGS[0].len()..];
    if !arg.starts_with("=")
    {
        errors.push(format!("error: compiler flag \"{}\" requires an argument.", COMPILER_FLAGS[0]));
    }
    else 
    {
        let parsed_arg: Result<u16, ParseIntError> = arg[1..].parse::<u16>();
        if let Err(ParseIntError{..}) = parsed_arg
        {
            errors.push(format!("error: compiler flag \"{}\" requires a numerical argument.", COMPILER_FLAGS[0]));
        }
        else 
        {
            ptr_size = parsed_arg.ok().expect("should be valid as error handled earlier.");
        }
        if ptr_size >= 2048
        {
            errors.push(format!("error: compiler flag \"{}\" requires an argument less than 2048.", COMPILER_FLAGS[0]));
        }
        if ptr_size < 8
        {
            errors.push(format!("error: compiler flag \"{}\" requires an argument that's at least 8.", COMPILER_FLAGS[0]));
        }
    }
    ptr_size
}

// Handle unrecognized flags in the command line.
fn handle_unrecognized_flag(arg: &String, errors: &mut Vec<String>)
{
    let index: Option<usize> = arg.find('=');
    let mut arg_substr: &str = &arg[..];
    if let Some(i) = index
    {
        arg_substr = &arg[..i]
    }
    errors.push(format!("error: unrecognized argument \"{arg_substr}\""));
}

// Gets the CLI info.
fn get_result(
    file_path: Option<String>, 
    errors: &mut Vec<String>, 
    ptr_size: u16, 
    multiple_file_error: bool)
    -> Result<CLIInfo, Vec<String>>
{
    let file_size: usize = get_file(&file_path, errors, multiple_file_error);

    if errors.len() > 0
    {
        Err(errors.clone())
    }
    else 
    {
        handle_compiler_flag_issues(&file_path, errors, ptr_size, file_size)
    }
}

fn get_file(file_path: &Option<String>, errors: &mut Vec<String>, multiple_file_error: bool) -> usize
{
    let mut file_size: usize = 0;
    if let Some(ref path) = file_path
    {
        let result: io::Result<String> = read_to_string(path);
        if result.is_err()
        {
            errors.push(format!("error: could not open file \"{path}\""));
        }
        else 
        {
            file_size = result.ok().expect("should be valid as error handled above").len();    
        }
    }
    else if !multiple_file_error
    {
        errors.push("error: no source file entered".to_string());
    }
    file_size
}

fn handle_compiler_flag_issues(
    file_path: &Option<String>, 
    errors: &mut Vec<String>, 
    ptr_size: u16, 
    file_size: usize) 
    -> Result<CLIInfo, Vec<String>>
{
    if let Some(file_path) = file_path
    {
        let ptr_size_bytes: u8 = (ptr_size / 8).try_into().expect("ptr_size maximum is less than 2048");
        if ptr_size % 8 != 0
        {
            println!("warning: argument of \"{}\" will be rounded down to the nearest multiple of 8", COMPILER_FLAGS[0]);
        }
        let ptr_size: usize = <u8 as Into<usize>>::into(ptr_size_bytes) * 8;
        if ptr_size > usize::BITS.try_into().expect("max value of usize must be less than the number of bits")
        {
            println!("warning: this program is being compiled for a {ptr_size}-bit machine, while this is only a {}-bit machine.", 
                usize::BITS);
        }
        else if ptr_size < usize::BITS.try_into().expect("max value of usize must be less than the number of bits")
            && file_size > 1 << ptr_size
        {
            errors.push(format!("error: the file is too big to compile for a {ptr_size}-bit machine"));
            return Err(errors.clone());
        }
        Ok(CLIInfo{ file_path: file_path.to_string(), cli_args: [ptr_size_bytes] })
    }
    else 
    {
        panic!("should never reach here since error list empty.")
    }
}