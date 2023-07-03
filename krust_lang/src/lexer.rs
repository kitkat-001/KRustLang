use std::env::args;
use std::fs::read_to_string;
use std::num::ParseIntError;
use std::panic::catch_unwind;
use std::io::Error;
use std::thread;

#[derive(Copy)]
#[derive(Clone)]
pub struct Token
{
    pub token_type: TokenType,
    pub line: usize,

    start: usize,
    length: usize
}

#[derive(Copy)]
#[derive(Clone)]
pub enum TokenType
{
    Plus, Minus, Star, Slash,
    LeftParen, RightParen,

    IntLiteral(u32),

    Error,
}

impl Token
{
    pub fn to_string<'a>(&'a self, source: &'a String) -> &str
    {
        &source[self.start..(self.start+self.length)]
    }
}

#[derive(Clone)]
pub enum LexerOutput
{
    LexInfo
    {
        file_text: String,
        tokens: Vec<Token>,
        errors: Vec<String>,
        can_compile: bool,
    },
    Failure(String)
}

pub fn lex() -> LexerOutput
{

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
    let file_text: Result<String, Error> = read_to_string(&file_path);
    if file_text.is_err()
    {
        return LexerOutput::Failure(String::from(format!("error: could not open file \"{file_path}\"")));
    }

    let file_text: String = file_text.ok().expect("should be valid as error handled earlier");
    let mut tokens: Vec<Token> = Vec::new();
    let mut errors: Vec<String> = Vec::new();
    let mut can_compile: bool = true;
    let mut index: usize = 0;
    let mut line: usize = 1;
    loop
    {
        let c: Option<char> = file_text.chars().nth(index);
        if let None = c
        {
            return LexerOutput::LexInfo{file_text, tokens, errors, can_compile};
        }
        let c: char = c.expect("should be valid as error handled earlier");

        if c == '+'
        {
            tokens.push(Token{token_type : TokenType::Plus, line, start : index, length : 1});
            index += 1;
        }
        else if c == '-' 
        {
            tokens.push(Token{token_type : TokenType::Minus, line, start : index, length : 1});
            index += 1;
        }
        else if c == '*' 
        {
            tokens.push(Token{token_type : TokenType::Star, line, start : index, length : 1});
            index += 1;
        }
        else if c == '/' 
        {
            tokens.push(Token{token_type : TokenType::Slash, line, start : index, length : 1});
            index += 1;
        }
        else if c == '(' 
        {
            tokens.push(Token{token_type : TokenType::LeftParen, line, start : index, length : 1});
            index += 1;
        }
        else if c == ')' 
        {
            tokens.push(Token{token_type : TokenType::RightParen, line, start : index, length : 1});
            index += 1;
        }
        else if c == ' ' || c == '\t' || c == '\n'|| c == '\r'
        {
            if c == '\n'
            {
                line += 1;
            }
            index += 1;
        }
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
                errors.push(String::from(format!("error (line {}): int literal \"{}\" must be at most {}", 
                    line, &file_text[index..(index+length)], 0x8000_0000u32)));
            }
            tokens.push(Token{token_type, line, start: index, length});
            index += length;
        }
        else
        {
            let mut length: usize = 1;
            while !is_token_seperator(&file_text.chars().nth(index + length))
            {
                length += 1;
            }
            tokens.push(Token{token_type: TokenType::Error, line, start: index, length});
            errors.push(String::from(format!("error (line {}): unrecognized token \"{}\"", line, &file_text[index..(index+length)])));
            can_compile = false;
            index += length;
        }
    }
}

fn get_int_literal_token_type(int_literal: Result<u32, ParseIntError>) -> TokenType
{
    if let Err(_) = int_literal
    { 
        return TokenType::Error; 
    }
    let value = int_literal.expect("should be valid as error handled on line 175.");

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


fn is_digit_option(c_option: &Option<char>) -> bool
{
    match c_option
    {
        None => false,
        Some(c) => c >= &'0' && c <= &'9'
    }
}

fn is_token_seperator(c_option: &Option<char>) -> bool
{
    match c_option
    {
        None => true,
        Some(c) => c == &' ' || c == &'\t' || c == &'\n' || c == &'\r'
            || c == &'(' || c == &')'
    }
}