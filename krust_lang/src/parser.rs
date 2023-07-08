//! The module for parsing the tokens and creating the AST.
use crate::lexer;
use lexer::{Token, LexerOutput, TokenType};

/// An enum represetning the possible types of expressions.
pub enum Expression
{
    Binary{left:  Box<Expression>, op: Token, right: Box<Expression>},
    Grouping{expr: Box<Expression>},
    Literal{token: Token},
    Unary{op: Token, expr: Box<Expression>},

    EOF,
    Null,
}

/// The output given by the parser.
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
    /// Convert the expression into a string.
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
                
            Expression::EOF => "EOF".to_string(),
            Expression::Null => "null".to_string(),
        }
    }

    fn is_eof(&self) -> bool
    {
        if let Expression::EOF = *self
        {
            true
        }
        else 
        {
            false
        }
    }
}

/// Parse the output from the lexer.
pub fn parse(lex_output: LexerOutput) -> ParserOutput
{
    match lex_output
    {
        LexerOutput::Failure(text) => ParserOutput::Failure(text),
        LexerOutput::LexInfo{file_path: _, file_text, tokens, errors, can_compile} =>
        {
            let mut can_compile: bool = can_compile;
            let mut errors: Vec<String> = errors.clone();
            let mut index: usize = 0;
            let expr: Expression = get_expression(&tokens.clone(), &mut errors, &mut can_compile, &mut index);
            if index < tokens.len() && tokens[index].token_type != TokenType::EOF
            {
                errors.push(format!("error (line {}:{}): expected EOF", tokens[index].line, tokens[index].col));
                can_compile = false;
            }
            ParserOutput::ParseInfo{file_text, expr, errors, can_compile}
        }
    }
}


// Get the expression from the token list.
fn get_expression(tokens: &Vec<Token>, errors: &mut Vec<String>, can_compile: &mut bool, index: &mut usize) -> Expression
{
    get_additive(&tokens, errors, can_compile, index)
}

// Get a primary expression (literals and grouping expressions).
fn get_primary(tokens: &Vec<Token>, errors: &mut Vec<String>, can_compile: &mut bool, index: &mut usize) -> Expression
{
    let token: Token = tokens[*index];
    *index += 1;
    match token.token_type
    {
        TokenType::IntLiteral(_) => Expression::Literal { token },
        TokenType::Error => Expression::Literal { token },
        TokenType::LeftParen => 
        {
            if tokens[*index].token_type == TokenType::RightParen
            {
                errors.push(format!("error (line {}:{}): expected expression within parenthesis", tokens[*index].line, tokens[*index].col));
                *can_compile = false;
                *index += 1;
                return Expression::Grouping { expr: Box::new(Expression::Null) };
            }
            let expr: Expression = get_expression(tokens, errors, can_compile, index);
            if let Expression::EOF = expr
            {
                errors.push(format!("error (line {}:{}): expected \')\' following \'(\'", tokens[*index-1].line, tokens[*index-1].col));
                *can_compile = false;
                return expr;
            }

            let expr: Box<Expression> = Box::new(expr);
            if tokens[*index].token_type == TokenType::RightParen
            {
                *index+=1;
            }
            else 
            {
                errors.push(format!("error (line {}:{}): expected \')\' following \'(\'", tokens[*index].line, tokens[*index].col));
                *can_compile = false;
            }
            Expression::Grouping { expr }
        },
        TokenType::EOF =>
        {
            errors.push(format!("error (line {}:{}): unexpected end of file", token.line, token.col));
            *can_compile = false;
            Expression::EOF
        },
        _ =>
        {
            errors.push(format!("error (line {}:{}): unexpected token", token.line, token.col));
            *can_compile = false;
            get_primary(tokens, errors, can_compile, index)
        }
    }
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
    else 
    {
        get_primary(tokens, errors, can_compile, index)
    }
}

// Get a multiplicative expression (*, /).
fn get_multiplicative(tokens: &Vec<Token>, errors: &mut Vec<String>, can_compile: &mut bool, index: &mut usize) -> Expression
{
    let mut expr: Expression = get_unary(tokens, errors, can_compile, index);
    while !expr.is_eof() &&
        (tokens[*index].token_type == TokenType::Star || tokens[*index].token_type == TokenType::Slash)
    {
        let op: Token = tokens[*index];
        *index += 1;
        let right: Expression = get_unary(tokens, errors, can_compile, index);
        let is_eof: bool = right.is_eof();
        expr = Expression::Binary{left: Box::new(expr), op, right: Box::new(right)};
        if is_eof
        {
            return expr;
        }
    }
    expr
} 


// Get a additive expression (+, -).
fn get_additive(tokens: &Vec<Token>, errors: &mut Vec<String>, can_compile: &mut bool, index: &mut usize) -> Expression
{
    let mut expr: Expression = get_multiplicative(tokens, errors, can_compile, index);
    while !expr.is_eof() &&
        (tokens[*index].token_type == TokenType::Plus || tokens[*index].token_type == TokenType::Minus)
    {
        let op: Token = tokens[*index];
        *index += 1;
        let right: Expression = get_multiplicative(tokens, errors, can_compile, index);
        let is_eof: bool = right.is_eof();
        expr = Expression::Binary{left: Box::new(expr), op, right: Box::new(right)};
        if is_eof
        {
            return expr;
        }
    }
    expr
} 