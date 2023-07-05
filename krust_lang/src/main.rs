mod lexer;
mod parser;

use lexer::{LexerOutput, lex};
use parser::{ParserOutput, parse};

fn main() {
    let lex_output: LexerOutput = lex();
    let parse_output: ParserOutput = parse(lex_output);
    
    match parse_output
    {
        ParserOutput::Failure(fail) =>
        {
            eprintln!("{}", fail);
        }
        ParserOutput::ParseInfo{file_text, expr, errors, can_compile} =>
        {
            println!("{}", expr.to_string(file_text.as_str()));
            for error in errors
            {
                eprintln!("{}", error);
            }
            if can_compile
            {
                println!("Can compile");
            }
            else {
                println!("could not compile due to above errors")
            }
        }
    }
}
