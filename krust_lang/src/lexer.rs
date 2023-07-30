//! The module for lexing the source file, i.e. splitting it up into tokens.

use std::fs::read_to_string;
use std::io::Error;
use std::num::ParseIntError;

/// A token representing an indivisible piece of the source code.
#[derive(Clone, Copy)]
pub struct Token
{
    pub token_type: TokenType,

    // For error handling.
    pub line: usize,
    pub col: usize,

    start: usize,
    length: usize
}

/// The allowed types of tokens.
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum TokenType
{
    // Single character tokens.
    Plus, Minus, Star, Slash,
    LeftParen, RightParen,

    // Literals
    IntLiteral(u32),

    EOF, // End of file.

    Error, // To be used when an invalid token is found.
}

impl Token
{
    /// Converts the token to a string slice given the text it came from.
    pub fn to_string<'a>(&'a self, source: &'a str) -> &str
    {
        if self.token_type == TokenType::EOF
        {
            "EOF"
        }
        else
        {
            &source[self.start..(self.start+self.length)]
        }
    }
}

/// The output given by the lexer.
pub struct LexerOutput
{
    pub file_text: String,
    pub tokens: Vec<Token>,
    pub errors: Vec<String>,
    pub can_compile: bool,
}

/// Lexes the file given in the command line.
pub fn lex(file_path: &str) -> LexerOutput
{
    // Prepare fields for output.
    let file_text: Result<String, Error> = read_to_string(file_path);
    let file_text: String = file_text.ok().expect("should be valid as error handled in command line reader");
    let mut tokens: Vec<Token> = Vec::new();
    let mut errors: Vec<String> = Vec::new();
    let mut can_compile: bool = true;

    // Values for the tokens.
    let mut index: usize = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;

    // Loop through each token until the end of the file is found.
    loop
    {
        let c: Option<char> = file_text.chars().nth(index);

        // EOF
        if let None = c
        {
            tokens.push(Token{token_type: TokenType::EOF, line, col, start: index, length: 0});
            return LexerOutput{file_text, tokens, errors, can_compile};
        }
        let c: char = c.expect("should be valid as error handled earlier");

        // Single character tokens.
        if c == '+'
        {
            tokens.push(Token{token_type : TokenType::Plus, line, col, start : index, length : 1});
            index += 1;
            col += 1;
        }
        else if c == '-' 
        {
            tokens.push(Token{token_type : TokenType::Minus, line, col, start : index, length : 1});
            index += 1;
            col += 1;
        }
        else if c == '*' 
        {
            tokens.push(Token{token_type : TokenType::Star, line, col, start : index, length : 1});
            index += 1;
            col += 1;
        }
        else if c == '/' 
        {
            tokens.push(Token{token_type : TokenType::Slash, line, col, start : index, length : 1});
            index += 1;
            col += 1;
        }
        else if c == '(' 
        {
            tokens.push(Token{token_type : TokenType::LeftParen, line, col, start : index, length : 1});
            index += 1;
            col += 1;
        }
        else if c == ')' 
        {
            tokens.push(Token{token_type : TokenType::RightParen, line, col, start : index, length : 1});
            index += 1;
            col += 1;
        }

        // White space
        else if c == ' ' || c == '\t' || c == '\n'|| c == '\r'
        {
            // Valid ways for writing new lines are "\n", "\r", or "\r\n".
            if c == '\r' || c == '\n'
            {
                line += 1;
                col = 1;
                if c == '\r' && file_text.chars().nth(index + 1) == Some('\n')
                {
                    index += 1;
                }
            }
            else 
            {    
                col += 1;
            }
            index += 1;
        }

        // Numbers
        else if c >= '0' && c <= '9'
        {
            let mut length: usize = 1;
            while is_digit_option(&file_text.chars().nth(index + length))
            {
                length += 1;
            }
            let int_literal: Result<u32, ParseIntError> = file_text[index..(index+length)].parse::<u32>();
            let token_type: TokenType = get_int_literal_token_type(int_literal);
            let token: Token = Token{token_type, line, col, start: index, length};
            if let TokenType::Error = token.token_type
            {
                can_compile = false;
                errors.push(format!("error (line {line}:{col}): int literal \"{}\" must be at most {}", 
                    token.to_string(&file_text), 0x8000_0000u32).to_string());
            }
            tokens.push(token);
            index += length;
            col += length;
        }

        // Error tokens.
        else
        {
            let mut length: usize = 1;
            while !is_token_separator(&file_text.chars().nth(index + length))
            {
                length += 1;
            }
            let token: Token = Token{token_type: TokenType::Error, line, col, start: index, length};
            errors.push(format!("error (line {line}:{col}): unrecognized token \"{}\"",
                token.to_string(&file_text)).to_string());
            tokens.push(token);
            can_compile = false;
            index += length;
            col += length;
        }
    }
}

/// Converts an integer literal to a token type.
fn get_int_literal_token_type(int_literal: Result<u32, ParseIntError>) -> TokenType
{
    if let Err(_) = int_literal
    { 
        return TokenType::Error; 
    }
    let value: u32 = int_literal.expect("should be valid as error handled earlier.");

    // 0x8000_0000 is the largest possible absolute value of an i32.
    if value > 0x8000_0000u32
    {
        return TokenType::Error;
    }
    else
    {
        return TokenType::IntLiteral(value);
    }
}

/// Returns whether or not the character stored in the option is a digit.
fn is_digit_option(c_option: &Option<char>) -> bool
{
    match c_option
    {
        None => false,
        Some(c) => c >= &'0' && c <= &'9'
    }
}

/// Returns whether or not the character stored in the option is a token seperator. <br/>
/// Token seperators include whitespace, valid brackets, and the end of the file.
fn is_token_separator(c_option: &Option<char>) -> bool
{
    match c_option
    {
        None => true,
        Some(c) => c == &' ' || c == &'\t' || c == &'\n' || c == &'\r'
            || c == &'(' || c == &')'
    }
}