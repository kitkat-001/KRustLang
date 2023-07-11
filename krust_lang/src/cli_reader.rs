//! The module for reading from the command line.

use std::cmp::min;
use std::env::args;
use std::fs::File;
use std::io;
use std::num::ParseIntError;
use std::panic::catch_unwind;
use std::thread;

/// The result from reading the command line.
pub enum CLIOutput
{
    // The standard output.
    CLIInfo
    {
        file_path: String,
        cli_args: [u8; 1],
    },

    // Returned only when compilation can not continue.
    Error(Vec<String>)
}

pub fn read_command_line() -> CLIOutput
{
    // File-reading errors.
    let input: thread::Result<Vec<String>> = catch_unwind(||{args().collect()});
    if input.is_err()
    {
        return CLIOutput::Error(vec!(String::from("error: could not read command line arguments")));
    }
    let input: Vec<String> = input.ok().expect("should be valid as error handled earlier");
    if input.len() == 1
    {
        return CLIOutput::Error(vec!("error: no command line arguments".to_string()));
    }
    
    let input: &[String] = &input[1..];
    let compiler_flags: [&str; 1] = ["-pointer_size"];
    let mut file_path: Option<String> = None;
    let mut ptr_size: u8 = min(usize::BITS, u8::MAX.into()).try_into().expect("should be valid as max value is u8");
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
        else if arg.starts_with(compiler_flags[0])
        {
            let arg: &str = &arg[compiler_flags[0].len()..];
            if !arg.starts_with("=")
            {
                errors.push(format!("error: compiler flag \"{}\" requires an argument.", compiler_flags[0]));
            }
            else 
            {
                let parsed_arg: Result<u8, ParseIntError> = arg[1..].parse::<u8>();
                if let Err(ParseIntError{..}) = parsed_arg
                {
                    errors.push(format!("error: compiler flag \"{}\" requires a numerical argument less than 256.", compiler_flags[0]));
                }
                else 
                {
                    ptr_size = parsed_arg.ok().expect("should be valid as error handled earlier.");
                }
            }
        }
        else
        {
            let index: Option<usize> = arg.find('=');
            let mut arg_substr: &str = &arg[..];
            if let Some(i) = index
            {
                arg_substr = &arg[..i]
            }
            errors.push(format!("error: unrecognized argument \"{arg_substr}\""));
        }
    }
    
    if let Some(ref path) = file_path
    {
        let result: io::Result<File> = File::open(path);
        if result.is_err()
        {
            errors.push(format!("error: could not open file \"{path}\""));
        }
    }
    else if !multiple_file_error
    {
        errors.push("error: no source file entered".to_string());
    }

    if errors.len() > 0
    {
        CLIOutput::Error(errors)
    }
    else 
    {
        if let Some(file_path) = file_path
        {
            CLIOutput::CLIInfo { file_path, cli_args: [ptr_size] }
        }
        else 
        {
            panic!("should never reach here since error list empty.")
        }
    }
}