mod lexer;

fn main() {
    let lex_output: lexer::LexerOutput = lexer::lex();

    match lex_output
    {
        lexer::LexerOutput::Failure(fail) =>
        {
            eprintln!("{}", fail);
        }
        lexer::LexerOutput::LexInfo{file_text, tokens, errors, ..} =>
        {
            for token in tokens
            {
                print!("\"{}\", ", token.to_string(&file_text))
            }
            println!();
            for error in errors
            {
                println!("{}", error);
            }
        }
    }
}
