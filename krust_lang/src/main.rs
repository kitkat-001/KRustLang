mod cli_reader;
mod lexer;
mod parser;

use cli_reader::{CLIOutput, read_command_line};
use lexer::{LexerOutput, lex};
use parser::{ParserOutput, parse};

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
        CLIOutput::CLIInfo { file_path, ..} =>
        {
            let lex_output: LexerOutput = lex(&file_path);
            let parse_output: ParserOutput = parse(lex_output);

            println!("{}", parse_output.expr.to_string(parse_output.file_text.as_str()));
            for error in parse_output.errors
            {
                eprintln!("{}", error);
            }
            if parse_output.can_compile
            {
                println!("Can compile");
            }
            else 
            {
                println!("could not compile due to above errors")
            }
        }
    }
}
