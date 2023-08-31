mod cli_reader;
mod lexer;
mod parser;
mod compiler;

use std::env::{current_dir, set_current_dir};
use std::fs::{File, rename};
use std::io::{prelude::*, Error};
use std::path::PathBuf;
use std::process::Command;
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
            let file_path: String = cli_output.file_path.clone().strip_suffix("txt")
                    .expect("file name is more than just suffix")
                    .to_string() + "exe";
            let result: Result<(), Error> = create_compiled_exe(&bytecode, &file_path);
            if let Err(_) = result
            {
                eprintln!("!fatal error; could not compile");
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

// Create the exe.
fn create_compiled_exe(bytecode: &Vec<u8>, file_path: &String) -> Result<(), Error>
{
    let mut file: File = File::create(concat!(env!("CARGO_MANIFEST_DIR"), "/", "src/program.rs"))?;
    file.write_all(create_code(bytecode).as_bytes())?;
    let curr_dir: PathBuf = current_dir()?;
    set_current_dir(env!("CARGO_MANIFEST_DIR"))?;
    Command::new("cargo").arg("build").output()?;
    rename(
        concat!(env!("CARGO_MANIFEST_DIR"), "/", "target/debug/program.exe"), 
        curr_dir.to_str().expect("should have a string value").to_string() + "/" 
            + file_path.as_str()
    )?;
    let mut file: File = File::create(concat!(env!("CARGO_MANIFEST_DIR"), "/", "src/program.rs"))?;
    file.write_all("fn main() {}".as_bytes())?;
    Command::new("cargo").arg("build").output()?;
    Ok(())
}

// Creates the rust code that can be compiled into an executable.
fn create_code(bytecode: &Vec<u8>) -> String
{
    format!("mod lexer;
mod parser;
mod compiler;
mod vm;
    
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