//! The module for lexing the source file, i.e. splitting it up into tokens.

use std::env::args;
use std::fs::read_to_string;
use std::num::ParseIntError;
use std::panic::catch_unwind;
use std::io::Error;
use std::thread;

/// A token representing an indivisible piece of the source code.
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
#[derive(PartialEq, Eq)]
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
pub enum LexerOutput
{
    /// The standard output. Should be returned in most cases.
    LexInfo
    {
        file_text: String,
        tokens: Vec<Token>,
        errors: Vec<String>,
        can_compile: bool,
    },
    /// Only to be returned when file can not be read. <br/>
    /// Errors that occur while lexing should be added to the <b>errors</b> field in the <b>LexInfo</b> struct.
    Failure(String)
}

/// Lexes the file given in the command line.
pub fn lex() -> LexerOutput
{
    // File-reading errors.
    let input: thread::Result<Vec<String>> = catch_unwind(||{args().collect()});
    if input.is_err()
    {
        return LexerOutput::Failure(String::from("error: could not read command line arguments"));
    }
    let input: Vec<String> = input.ok().expect("should be valid as error handled earlier");
    if input.len() > 2
    {
        return LexerOutput::Failure(String::from("error: too many command line arguments"));
    }
    if input.len() == 1
    {
        return LexerOutput::Failure("error: not enough command line arguments".to_string());
    }
    let file_path: &String = &input[1];
    let file_text: Result<String, Error> = read_to_string(file_path);
    if file_text.is_err()
    {
        return LexerOutput::Failure(String::from(format!("error: could not open file \"{file_path}\"")));
    }

    // Prepare fields for output.
    let file_text: String = file_text.ok().expect("should be valid as error handled earlier");
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
            return LexerOutput::LexInfo{file_text, tokens, errors, can_compile};
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
            else if c == '\t'
            {
                col += 4 - ((col - 1) % 4);
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
            if let TokenType::Error = token_type
            {
                can_compile = false;
                errors.push(String::from(format!("error ({file_path}:{line}:{col}): int literal \"{}\" must be at most {}", 
                    &file_text[index..(index+length)], 0x8000_0000u32)));
            }
            tokens.push(Token{token_type, line, col, start: index, length});
            index += length;
            col += length;
        }

        // Error tokens.
        else
        {
            let mut length: usize = 1;
            while !is_token_seperator(&file_text.chars().nth(index + length))
            {
                length += 1;
            }
            tokens.push(Token{token_type: TokenType::Error, line, col, start: index, length});
            errors.push(String::from(format!("error ({file_path}:{line}:{col}): unrecognized token \"{}\"",
                &file_text[index..(index+length)])));
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
fn is_token_seperator(c_option: &Option<char>) -> bool
{
    match c_option
    {
        None => true,
        Some(c) => c == &' ' || c == &'\t' || c == &'\n' || c == &'\r'
            || c == &'(' || c == &')'
    }
}