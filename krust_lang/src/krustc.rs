mod cli_reader;
mod lexer;
mod parser;
mod compiler;

use std::fs::File;
use std::io::{prelude::*, Error};
use cli_reader::{CLIInfo, read_command_line};
use lexer::{LexerOutput, lex};
use parser::{ParserOutput, parse};
use compiler::{CompilerOutput, compile};

fn main() {
    let cli_output: Result<CLIInfo, Vec<String>>  = read_command_line();
    if cli_output.is_err()
    {
        for error in cli_output.err().expect("checked by if statement")
        {
            eprintln!("{}", error);
        }
    }
    else
    {
        let cli_output: CLIInfo = cli_output.ok().expect("checked by if statement");
        let compiler_output: CompilerOutput = generate_bytecode(
            &cli_output.file_path,
            cli_output.cli_args);
        for err in compiler_output.errors
        {
            eprintln!("{err}");
        }
        if let Some(bytecode) = compiler_output.bytecode
        {
            let file: Result<File, Error> = File::create(format!("{}/../program.rs", file!()));
            if file.is_err()
            {
                eprintln!("fatal error: could not create executable");
                return;
            }
            let result: Result<(), Error> = file.expect("checked by if statement").write_all(create_code(&bytecode).as_bytes());
            if result.is_err()
            {
                eprintln!("fatal error: could not create executable");
                return;
            }

        }
    }
}

// Produces bytecode from the file.
fn generate_bytecode(file_path: &String, cli_args: [u8;1]) -> CompilerOutput
{
    let lex_output: LexerOutput = lex(file_path);
    let parse_output: ParserOutput = parse(lex_output);
    compile(parse_output, cli_args)
}

// Creates the rust code that can be compiled into an executable.
fn create_code(bytecode: &Vec<u8>) -> String
{
    format!("mod vm;
    
fn main(){{
    let bytecode: Vec<u8> = vec!{:?};
    let out_err: (Vec<String>, Option<String>) = vm::run(&bytecode);
    for string in out_err.0
    {{
        println!(\"{{}}\", string);
    }}
    if let Some(err) = out_err.1
    {{
        eprintln!(\"{{}}\", err);
    }}
}}", bytecode)
}