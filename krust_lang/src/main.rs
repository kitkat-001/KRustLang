mod cli_reader;
mod lexer;
mod parser;
mod compiler;
mod vm;

use cli_reader::{CLIOutput, read_command_line};
use lexer::{LexerOutput, lex};
use parser::{ParserOutput, parse};
use compiler::{CompilerOutput, compile};

fn main() {
    let cli_output: CLIOutput = read_command_line();
    match cli_output
    {
        CLIOutput::Error(errors) =>
        {
            for error in errors
            {
                eprintln!("{}", error);
            }
        }
        CLIOutput::CLIInfo { file_path, cli_args} =>
        {
            let output: (Vec<String>, Vec<String>) = run(file_path, cli_args);
            for string in output.0
            {
                println!("{}", string);
            }
            for string in output.1
            {
                println!("{}", string);
            }
        }
    }
}

// Runs the code in the file.
// TODO: Don't save all the printing till the end, instead print when printing should happen.
fn run(file_path: String, cli_args: [u8; 1]) -> (Vec<String>, Vec<String>)
{
    let lex_output: LexerOutput = lex(&file_path);
    let parse_output: ParserOutput = parse(lex_output);
    let compile_ouput: CompilerOutput = compile(parse_output, cli_args);
    let mut output: Vec<String> = Vec::new();
    let mut err: Vec<String> = Vec::new();

    for error in compile_ouput.errors
    {
        err.push(error);
    }
    if let Some(bytecode) = compile_ouput.bytecode
    {
        let output_with_err: (Vec<String>, Option<String>) = vm::run(&bytecode);
        output.append(&mut output_with_err.0.clone());
        if let Some(err_value) = output_with_err.1
        {
            err.push(err_value);
        }
    }
    else 
    {
        err.push("could not compile due to above errors".to_string());
    }

    (output, err)
}
