//! The module for parsing the tokens and creating the AST.
use crate::lexer;
use lexer::{Token, LexerOutput, TokenType};

// An enum represetning the possible types of expressions.
pub enum Expression
{
    Binary{left:  Box<Expression>, op: Token, right: Box<Expression>},
    Grouping{expr: Box<Expression>},
    Literal{token: Token},
    Unary{op: Token, expr: Box<Expression>},
}

// The output given by the parser.
pub enum ParserOutput
{
    ParseInfo
    {
        file_text: String,
        expr: Expression,
        errors: Vec<String>,
        can_compile: bool,
    },
    Failure(String)
}

impl Expression
{
    // Convert the expression into a string.
    pub fn to_string(&self, source: &str) -> String
    {
        match &self
        {
            Expression::Binary { left, op, right } 
                => format!("{}({}, {})", op.to_string(source), (*left).to_string(source), (*right).to_string(source)),
            Expression::Grouping { expr }
                => format!("({})", (*expr).to_string(source)),
            Expression::Literal { token } => token.to_string(source).to_string(),
            Expression::Unary { op, expr } 
                => format!("{}({})", op.to_string(source), (*expr).to_string(source)),
        }
    }
}

// Parse the output from the lexer.
pub fn parse(lex_output: LexerOutput) -> ParserOutput
{
    match lex_output
    {
        LexerOutput::Failure(text) => ParserOutput::Failure(text),
        LexerOutput::LexInfo{file_path: _, file_text, tokens, errors, can_compile} =>
        {
            let mut can_compile: bool = can_compile;
            let mut errors: Vec<String> = errors.clone();
            let expr: Expression = get_expression(&tokens.clone(), &mut errors, &mut can_compile, &mut 0);
            ParserOutput::ParseInfo{file_text, expr, errors, can_compile}
        }
    }
}


// Get the expression from the token list.
fn get_expression(tokens: &Vec<Token>, errors: &mut Vec<String>, can_compile: &mut bool, index: &mut usize) -> Expression
{
    get_additive(&tokens, errors, can_compile, index)
}

// Get an additive expression (+, -).
fn get_additive(tokens: &Vec<Token>, errors: &mut Vec<String>, can_compile: &mut bool, index: &mut usize) -> Expression
{
    let mut expr: Expression = get_multiplicative(tokens, errors, can_compile, index);
    while tokens[*index + 1].token_type == TokenType::Plus || tokens[*index + 1].token_type == TokenType::Minus
    {
        *index += 1;
        let op: Token = tokens[*index];
        *index += 1;
        let right: Expression = get_multiplicative(tokens, errors, can_compile, index);
        expr = Expression::Binary{left: Box::new(expr), op, right: Box::new(right)}
    }
    expr
}

// Get a multiplicative expression (*, /).
fn get_multiplicative(tokens: &Vec<Token>, errors: &mut Vec<String>, can_compile: &mut bool, index: &mut usize) -> Expression
{
    let mut expr: Expression = get_unary(tokens, errors, can_compile, index);
    while tokens[*index + 1].token_type == TokenType::Star || tokens[*index + 1].token_type == TokenType::Slash
    {
        *index += 1;
        let op: Token = tokens[*index];
        *index += 1;
        let right: Expression = get_unary(tokens, errors, can_compile, index);
        expr = Expression::Binary{left: Box::new(expr), op, right: Box::new(right)}
    }
    expr
} 

// Get a unary expression.
fn get_unary(tokens: &Vec<Token>, errors: &mut Vec<String>, can_compile: &mut bool, index: &mut usize) -> Expression
{
    if tokens[*index].token_type == TokenType::Minus
    {
        let op: Token = tokens[*index];
        *index += 1;
        let expr: Expression = get_unary(tokens, errors, can_compile, index);
        Expression::Unary { op, expr: Box::new(expr) }
    }
    else {
        get_primary(tokens, errors, can_compile, index)
    }
}

// Get a primary expression (literals and grouping expressions).
fn get_primary(tokens: &Vec<Token>, errors: &mut Vec<String>, can_compile: &mut bool, index: &mut usize) -> Expression
{
    match tokens[*index].token_type
    {
        TokenType::IntLiteral(_) => Expression::Literal { token: tokens[*index] },
        TokenType::Error => Expression::Literal { token: tokens[*index] },
        TokenType::LeftParen =>
        {
            *index += 1;
            let expr: Expression = get_expression(tokens, errors, can_compile, index);
            if tokens[*index].token_type != TokenType::RightParen
            {
                errors.push(format!("error (line {}:{}): Expected \')\' following \'(\'", tokens[*index].line, tokens[*index].col));
                *can_compile = false;
            }
            else 
            {
                *index += 1;
            }
            Expression::Grouping { expr: Box::new(expr) }
        },
        _ =>
        {
            errors.push(format!("error (line {}:{}): Unexpected token", 
                tokens[*index].line, tokens[*index].col));
            *can_compile = false;
            *index += 1;
            get_primary(tokens, errors, can_compile, index)
        }
    }
}