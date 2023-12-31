#![deny(clippy::all)]
#![deny(clippy::pedantic)]

use krust::cli_reader::{read_command_line, CLIInfo};
use krust::compiler::{compile, CompilerOutput};
use krust::lexer::{lex, LexerOutput};
use krust::parser::{parse, ParserOutput};
use krust::util::log::{ErrorType, Log, LogType};
use std::env::{current_dir, set_current_dir};
use std::fs::{rename, File, read_to_string};
use std::io::{prelude::*, Error};
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let cli_output: (Option<CLIInfo>, Vec<Log>) = read_command_line();
    for log in cli_output.1 {
        eprintln!("{log}");
    }

    if cli_output.0.is_some() {
        let cli_output: CLIInfo = cli_output.0.expect("checked by if statement");
        let compiler_output: CompilerOutput =
            generate_bytecode(&cli_output.file_path, cli_output.cli_args);
        let mut logs: Vec<Log> = compiler_output.logs.clone();
        if let Some(bytecode) = compiler_output.bytecode {
            let file_path: String = cli_output
                .file_path
                .strip_suffix("txt")
                .expect("file name is more than just suffix")
                .to_string()
                + "exe";
            let result: Result<(), Error> = create_compiled_exe(&bytecode, &file_path);
            if result.is_err() {
                logs.push(Log {
                    log_type: LogType::Error(ErrorType::FatalError),
                    line_and_col: None,
                });
            }
        } else {
            logs.push(Log {
                log_type: LogType::Error(ErrorType::CantCompile),
                line_and_col: None,
            });
        }
        for log in logs {
            eprintln!("{log}");
        }
    }
}

// Produces bytecode from the file.
fn generate_bytecode(file_path: &str, cli_args: [u8; 2]) -> CompilerOutput {
    let lex_output: LexerOutput = lex(read_to_string(file_path).expect("should be valid as error handled in command line reader"));
    let parse_output: ParserOutput = parse(lex_output);
    compile(parse_output, cli_args)
}

// Create the exe.
fn create_compiled_exe(bytecode: &Vec<u8>, file_path: &str) -> Result<(), Error> {
    let mut file: File = File::create(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/",
        "src/bin/program.rs"
    ))?;
    file.write_all(create_code(bytecode).as_bytes())?;
    let curr_dir: PathBuf = current_dir()?;
    set_current_dir(env!("CARGO_MANIFEST_DIR"))?;
    Command::new("cargo").arg("build").output()?;
    rename(
        concat!(env!("CARGO_MANIFEST_DIR"), "/", "target/debug/program.exe"),
        curr_dir
            .to_str()
            .expect("should have a string value")
            .to_string()
            + "/"
            + file_path,
    )?;
    let mut file: File = File::create(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/",
        "src/bin/program.rs"
    ))?;
    file.write_all(b"fn main() {}")?;
    Command::new("cargo").arg("build").output()?;
    Ok(())
}

// Creates the rust code that can be compiled into an executable.
fn create_code(bytecode: &Vec<u8>) -> String {
    format!(
        "use krust::vm;
    
fn main(){{
    let bytecode: Vec<u8> = vec!{bytecode:?};
    vm::run(&bytecode);
}}"
    )
}
