//! The module for lexing the source file, i.e. splitting it up into tokens.

mod trie;

use crate::log;
use log::{ErrorType, Log, LogType};
use std::collections::HashMap;
use std::fs::read_to_string;
use std::io::Error;
use std::num::ParseIntError;

use trie::Node;

/// A token representing an indivisible piece of the source code.
#[derive(Clone, Copy)]
pub struct Token {
    pub token_type: TokenType,

    // For error handling.
    pub line: usize,
    pub col: usize,

    start: usize,
    length: usize,
}

/// The allowed types of tokens.
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    // Single character tokens.
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Tilde,
    ExclamationMark,
    Ampersand,
    Caret,
    Bar,
    LeftParen,
    RightParen,

    // Multi-character tokens.
    LeftShift,
    RightShift,
    Equality,
    Inequality,

    // Literals
    IntLiteral(u32),

    // Keywords
    True,
    False,

    EOF, // End of file.

    Error, // To be used when an invalid token is found.
}

impl Token {
    /// Converts the token to a string given the text it came from.
    #[must_use]
    pub fn to_string<'a>(&'a self, source: &'a str) -> String {
        if self.token_type == TokenType::EOF {
            "EOF".to_string()
        } else {
            source[self.start..(self.start + self.length)].to_string()
        }
    }
}

/// The output given by the lexer.
pub struct LexerOutput {
    pub file_text: String,
    pub tokens: Vec<Token>,
    pub logs: Vec<Log>,
}

/// Lexes the file given in the command line.
#[must_use]
pub fn lex(file_path: &str) -> LexerOutput {
    // Prepare fields for output.
    let file_text: Result<String, Error> = read_to_string(file_path);
    let file_text: String =
        file_text.expect("should be valid as error handled in command line reader");
    let mut tokens: Vec<Token> = Vec::new();
    let mut logs: Vec<Log> = Vec::new();

    // Values for the tokens.
    let mut index: usize = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;

    // Create the trie for the lexer so it only has to be made once.
    let trie: Node<char, TokenType> = Node::new_with_string(vec![
        ("true".to_string(), TokenType::True),
        ("false".to_string(), TokenType::False),
    ]);

    // Loop through each token until the end of the file is found.
    loop {
        let output: Option<LexerOutput> = get_token(
            &file_text,
            &mut tokens,
            &mut logs,
            &mut index,
            &mut line,
            &mut col,
            &trie,
        );
        if let Some(out) = output {
            return out;
        }
    }
}

// Gets the next token in the code.
fn get_token(
    file_text: &String,
    tokens: &mut Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    line: &mut usize,
    col: &mut usize,
    trie: &Node<char, TokenType>,
) -> Option<LexerOutput> {
    let c: Option<char> = file_text.chars().nth(*index);
    let token_dict: HashMap<char, TokenType> = HashMap::from([
        ('+', TokenType::Plus),
        ('-', TokenType::Minus),
        ('*', TokenType::Star),
        ('/', TokenType::Slash),
        ('%', TokenType::Percent),
        ('~', TokenType::Tilde),
        ('&', TokenType::Ampersand),
        ('^', TokenType::Caret),
        ('|', TokenType::Bar),
        ('(', TokenType::LeftParen),
        (')', TokenType::RightParen),
    ]);

    // EOF
    if c.is_none() {
        tokens.push(Token {
            token_type: TokenType::EOF,
            line: *line,
            col: *col,
            start: *index,
            length: 0,
        });
        return Some(LexerOutput {
            file_text: file_text.to_string(),
            tokens: tokens.clone(),
            logs: logs.clone(),
        });
    }
    let c: char = c.expect("should be valid as error handled earlier");
    // Single character tokens.
    if token_dict.contains_key(&c) {
        tokens.push(Token {
            token_type: token_dict[&c],
            line: *line,
            col: *col,
            start: *index,
            length: 1,
        });
        *index += 1;
        *col += 1;
    } else if c == ' ' || c == '\t' || c == '\n' || c == '\r' {
        handle_white_space(file_text, c, line, col, index);
    } else if handle_equals(file_text, tokens, line, col, index) {
    } else if handle_shift(file_text, tokens, line, col, index) {
    } else if c.is_ascii_digit() {
        handle_number(file_text, tokens, logs, line, col, index);
    } else {
        handle_other(file_text, tokens, logs, line, col, index, trie);
    }

    None
}

// Handles white space.
fn handle_white_space(
    file_text: &str,
    c: char,
    line: &mut usize,
    col: &mut usize,
    index: &mut usize,
) {
    // Valid ways for writing new lines are "\n", "\r", or "\r\n".
    if c == '\r' || c == '\n' {
        *line += 1;
        *col = 1;
        if c == '\r' && file_text.chars().nth(*index + 1) == Some('\n') {
            *index += 1;
        }
    } else {
        *col += 1;
    }
    *index += 1;
}

// Handles tokens using the equals sign.
fn handle_equals(
    file_text: &str,
    tokens: &mut Vec<Token>,
    line: &mut usize,
    col: &mut usize,
    index: &mut usize,
) -> bool {
    let c: Option<char> = file_text.chars().nth(*index);
    if c == Some('=') {
        let c: Option<char> = file_text.chars().nth(*index + 1);
        if c == Some('=') {
            let token: Token = Token {
                token_type: TokenType::Equality,
                line: *line,
                col: *col,
                start: *index,
                length: 2,
            };
            tokens.push(token);
            *index += 2;
            *col += 2;
            return true;
        }
    }
    else if c == Some('!') {
        let c: Option<char> = file_text.chars().nth(*index + 1);
        if c == Some('=') {
            let token: Token = Token {
                token_type: TokenType::Inequality,
                line: *line,
                col: *col,
                start: *index,
                length: 2,
            };
            tokens.push(token);
            *index += 2;
            *col += 2;
            return true;
        }
        let token: Token = Token {
            token_type: TokenType::ExclamationMark,
            line: *line,
            col: *col,
            start: *index,
            length: 1,
        };
        tokens.push(token);
        *index += 1;
        *col += 1;
        return true;
    }
    false
}

// Handles shift tokens.
fn handle_shift(
    file_text: &str,
    tokens: &mut Vec<Token>,
    line: &mut usize,
    col: &mut usize,
    index: &mut usize,
) -> bool {
    let c: Option<char> = file_text.chars().nth(*index);
    if c == Some('<') {
        let c: Option<char> = file_text.chars().nth(*index + 1);
        if c == Some('<') {
            let token: Token = Token {
                token_type: TokenType::LeftShift,
                line: *line,
                col: *col,
                start: *index,
                length: 2,
            };
            tokens.push(token);
            *index += 2;
            *col += 2;
            return true;
        }
    } else if c == Some('>') {
        let c: Option<char> = file_text.chars().nth(*index + 1);
        if c == Some('>') {
            let token: Token = Token {
                token_type: TokenType::RightShift,
                line: *line,
                col: *col,
                start: *index,
                length: 2,
            };
            tokens.push(token);
            *index += 2;
            *col += 2;
            return true;
        }
    }
    false
}

// Handles numerical tokens.
fn handle_number(
    file_text: &str,
    tokens: &mut Vec<Token>,
    logs: &mut Vec<Log>,
    line: &mut usize,
    col: &mut usize,
    index: &mut usize,
) {
    let mut length: usize = 1;
    while is_digit_option(file_text.chars().nth(*index + length)) {
        length += 1;
    }
    let int_literal: Result<u32, ParseIntError> =
        file_text[*index..(*index + length)].parse::<u32>();
    let token_type: TokenType = get_int_literal_token_type(int_literal);
    let token: Token = Token {
        token_type,
        line: *line,
        col: *col,
        start: *index,
        length,
    };
    if token.token_type == TokenType::Error {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::UnrepresentableIntegerLiteral(
                token.to_string(file_text),
            )),
            line_and_col: Some((*line, *col)),
        });
    }
    tokens.push(token);
    *index += length;
    *col += length;
}

// Converts an integer literal to a token type.
fn get_int_literal_token_type(int_literal: Result<u32, ParseIntError>) -> TokenType {
    if int_literal.is_err() {
        return TokenType::Error;
    }
    let value: u32 = int_literal.expect("should be valid as error handled earlier.");

    // 0x8000_0000 is the largest possible absolute value of an i32.
    if value > 0x8000_0000_u32 {
        TokenType::Error
    } else {
        TokenType::IntLiteral(value)
    }
}

// Returns whether or not the character stored in the option is a digit.
fn is_digit_option(c_option: Option<char>) -> bool {
    match c_option {
        None => false,
        Some(c) => c.is_ascii_digit(),
    }
}

// Handles keywords and unexpected characters/tokens.
fn handle_other(
    file_text: &str,
    tokens: &mut Vec<Token>,
    logs: &mut Vec<Log>,
    line: &mut usize,
    col: &mut usize,
    index: &mut usize,
    trie: &Node<char, TokenType>,
) {
    let mut length: usize = 1;
    while !is_token_separator(file_text.chars().nth(*index + length)) {
        length += 1;
    }
    let token_string: String = file_text[*index..*index + length].to_string();
    let token_type: Option<TokenType> = trie.search_with_string(&token_string);
    let token_type: TokenType = if let Some(t) = token_type {
        t
    } else {
        TokenType::Error
    };
    let token: Token = Token {
        token_type,
        line: *line,
        col: *col,
        start: *index,
        length,
    };
    if token_type == TokenType::Error {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::UnrecognizedToken(token.to_string(file_text))),
            line_and_col: Some((*line, *col)),
        });
    }
    tokens.push(token);
    *index += length;
    *col += length;
}

// Returns whether or not the character stored in the option is a token seperator. <br/>
// Token seperators include whitespace, valid brackets, and the end of the file.
fn is_token_separator(c_option: Option<char>) -> bool {
    match c_option {
        None => true,
        Some(c) => !(c.is_ascii_alphanumeric() || c == '_'),
    }
}
