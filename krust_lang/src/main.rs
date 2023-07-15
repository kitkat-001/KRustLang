mod cli_reader;
mod lexer;
mod parser;
mod compiler;

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
            let lex_output: LexerOutput = lex(&file_path);
            let parse_output: ParserOutput = parse(lex_output);
            let compile_ouput: CompilerOutput = compile(parse_output, cli_args);

            for error in compile_ouput.errors
            {
                eprintln!("{}", error);
            }
            if let Some(bytecode) = compile_ouput.bytecode
            {
                for byte in bytecode
                {
                    print!("{}, ", byte);
                }
            }
            else 
            {
                println!("could not compile due to above errors")
            }
        }
    }
}
