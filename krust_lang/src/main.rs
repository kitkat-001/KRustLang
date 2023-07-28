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
            let output: (String, String) = run(file_path, cli_args);
            println!("{}", output.0.as_str());
            eprintln!("{}", output.1.as_str());
        }
    }
}

fn run(file_path: String, cli_args: [u8; 1]) -> (String, String)
{
    let lex_output: LexerOutput = lex(&file_path);
    let parse_output: ParserOutput = parse(lex_output);
    let compile_ouput: CompilerOutput = compile(parse_output, cli_args);
    let mut output: String = String::new();
    let mut err: String = String::new();

    for error in compile_ouput.errors
    {
        err += format!("{}\n", error).as_str();
    }
    if let Some(bytecode) = compile_ouput.bytecode
    {
        output += vm::run(&bytecode).0.as_str();
        err += vm::run(&bytecode).1.as_str();
    }
    else 
    {
        err += "could not compile due to above errors";
    }

    (output, err)
}
