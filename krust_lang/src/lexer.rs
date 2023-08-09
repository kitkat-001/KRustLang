//! The module for lexing the source file, i.e. splitting it up into tokens.

use std::collections::HashMap;
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
        let output: Option<LexerOutput> = get_token(
            &file_text,
            &mut tokens,
            &mut errors,
            &mut can_compile,
            &mut index,
            &mut line,
            &mut col
        );
        if output.is_some() { return output.expect("checked by if statement"); }
    }
}

// Gets the next token in the code.
fn get_token(
    file_text: &String, 
    tokens: &mut Vec<Token>, 
    errors: &mut Vec<String>,
    can_compile: &mut bool,
    index: &mut usize,
    line: &mut usize,
    col: &mut usize
) -> Option<LexerOutput>
{
    let c: Option<char> = file_text.chars().nth(*index);
    let token_dict: HashMap<char, TokenType> = HashMap::from([
        ('+', TokenType::Plus),
        ('-', TokenType::Minus),
        ('*', TokenType::Star),
        ('/', TokenType::Slash),
    
        ('(', TokenType::LeftParen),
        (')', TokenType::RightParen),
    ]);

    // EOF
    if let None = c
    {
        tokens.push(Token{token_type: TokenType::EOF, line: *line, col: *col, start: *index, length: 0});
        return Some(LexerOutput{
            file_text: file_text.to_string(), 
            tokens: tokens.clone(), 
            errors: errors.clone(), 
            can_compile: *can_compile});
    }
    let c: char = c.expect("should be valid as error handled earlier");
    // Single character tokens.
    if token_dict.contains_key(&c)
    {
        tokens.push(Token{token_type : token_dict[&c], line: *line, col: *col, start : *index, length : 1});
        *index += 1;
        *col += 1;
    }
    else if c == ' ' || c == '\t' || c == '\n'|| c == '\r'
    {
        handle_white_space(file_text, c, line, col, index);
    }
    else if c >= '0' && c <= '9'
    {
        handle_number(file_text, tokens, errors, can_compile, line, col, index);
    }
    else
    {
        handle_error(file_text, tokens, errors, can_compile, line, col, index)
    }

    None
}

// Handles white space.
fn handle_white_space(file_text: &String, c: char, line: &mut usize, col: &mut usize, index: &mut usize)
{
    // Valid ways for writing new lines are "\n", "\r", or "\r\n".
    if c == '\r' || c == '\n'
    {
        *line += 1;
        *col = 1;
        if c == '\r' && file_text.chars().nth(*index + 1) == Some('\n')
        {
            *index += 1;
        }
    }
    else 
    {    
        *col += 1;
    }
    *index += 1;
}

// Handles numerical tokens.
fn handle_number(
    file_text: &String, 
    tokens: &mut Vec<Token>,
    errors: &mut Vec<String>,
    can_compile: &mut bool,  
    line: &mut usize,
    col: &mut usize, 
    index: &mut usize,  
)
{
    let mut length: usize = 1;
    while is_digit_option(&file_text.chars().nth(*index + length))
    {
        length += 1;
    }
    let int_literal: Result<u32, ParseIntError> = file_text[*index..(*index+length)].parse::<u32>();
    let token_type: TokenType = get_int_literal_token_type(int_literal);
    let token: Token = Token{token_type, line: *line, col: *col, start: *index, length};
    if let TokenType::Error = token.token_type
    {
        *can_compile = false;
        errors.push(format!("error (line {}:{}): int literal \"{}\" must be at most {}", 
            *line, *col, token.to_string(&file_text), 0x8000_0000u32).to_string());
    }
    tokens.push(token);
    *index += length;
    *col += length;
}

// Converts an integer literal to a token type.
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

// Returns whether or not the character stored in the option is a digit.
fn is_digit_option(c_option: &Option<char>) -> bool
{
    match c_option
    {
        None => false,
        Some(c) => c >= &'0' && c <= &'9'
    }
}

// Handles unexpected characters/tokens.
fn handle_error(
    file_text: &String, 
    tokens: &mut Vec<Token>,
    errors: &mut Vec<String>,
    can_compile: &mut bool,  
    line: &mut usize,
    col: &mut usize, 
    index: &mut usize
)
{
    let mut length: usize = 1;
    while !is_token_separator(&file_text.chars().nth(*index + length))
    {
        length += 1;
    }
    let token: Token = Token{token_type: TokenType::Error, line: *line, col: *col, start: *index, length};
    errors.push(format!("error (line {}:{}): unrecognized token \"{}\"",
        *line, *col, token.to_string(&file_text)).to_string());
    tokens.push(token);
    *can_compile = false;
    *index += length;
    *col += length;
}

// Returns whether or not the character stored in the option is a token seperator. <br/>
// Token seperators include whitespace, valid brackets, and the end of the file.
fn is_token_separator(c_option: &Option<char>) -> bool
{
    match c_option
    {
        None => true,
        Some(c) => c == &' ' || c == &'\t' || c == &'\n' || c == &'\r'
            || c == &'(' || c == &')'
    }
}