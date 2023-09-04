//! The module for parsing the tokens and creating the AST.
use crate::{log, lexer};
use log::{Log, LogType, ErrorType};
use lexer::{Token, LexerOutput, TokenType};

/// An enum represetning the possible types of expressions.
#[derive(Clone)]
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
pub struct ParserOutput
{
    pub file_text: String,
    pub expr: Expression,
    pub logs: Vec<Log>,
}

impl Expression
{
    #[allow(dead_code)]
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
    let mut logs: Vec<Log> = lex_output.logs.clone();
    let mut index: usize = 0;
    let tokens: Vec<Token> = lex_output.tokens;
    let expr: Expression = get_expression(&tokens.clone(), &mut logs, &mut index);
    if index < tokens.len() && tokens[index].token_type != TokenType::EOF
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::ExpectedEOF), 
            line_and_col: Some((tokens[index].line, tokens[index].col))});
    }
    improve_ast(Box::new(expr.clone()), None, &mut logs);
    ParserOutput{file_text: lex_output.file_text.clone(), expr, logs}
}


// Get the expression from the token list.
fn get_expression(tokens: &Vec<Token>, logs: &mut Vec<Log>, index: &mut usize) -> Expression
{
    get_or(tokens, logs, index)
}

// Get a primary expression (literals and grouping expressions).
fn get_primary(tokens: &Vec<Token>, logs: &mut Vec<Log>, index: &mut usize) -> Expression
{
    let token: Token = tokens[*index];
    *index += 1;
    match token.token_type
    {
        TokenType::IntLiteral(_) => Expression::Literal { token },
        TokenType::Error => Expression::Literal { token },
        TokenType::LeftParen => 
        {
            handle_paren(tokens, logs, index)
        },
        TokenType::EOF =>
        {
            logs.push(Log{log_type: LogType::Error(ErrorType::UnexpectedEOF), 
                line_and_col: Some((token.line, token.col))});
            Expression::EOF
        },
        _ =>
        {
            logs.push(Log{log_type: LogType::Error(ErrorType::UnexpectedToken), 
                line_and_col: Some((token.line, token.col))});
            get_expression(tokens, logs, index)
        }
    }
}

// Handles primary expressions that use parentheses.
fn handle_paren(tokens: &Vec<Token>, logs: &mut Vec<Log>, index: &mut usize) -> Expression
{
    if tokens[*index].token_type == TokenType::RightParen
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::ExpectedExpressionInParens), 
            line_and_col: Some((tokens[*index].line, tokens[*index].col))});
        *index += 1;
        return Expression::Grouping { expr: Box::new(Expression::Null) };
    }
    let expr: Expression = get_expression(tokens, logs, index);
    if let Expression::EOF = expr
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::ExpectedCloseParen), 
            line_and_col: Some((tokens[*index-1].line, tokens[*index-1].col))});
        return expr;
    }

    let expr: Box<Expression> = Box::new(expr);
    if tokens[*index].token_type == TokenType::RightParen
    {
        *index+=1;
    }
    else 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::ExpectedCloseParen), 
            line_and_col: Some((tokens[*index].line, tokens[*index].col))});
    }
    Expression::Grouping { expr }
}

// Get a unary expression.
fn get_unary(tokens: &Vec<Token>, logs: &mut Vec<Log>, index: &mut usize) -> Expression
{
    if tokens[*index].token_type == TokenType::Minus || tokens[*index].token_type == TokenType::Tilde
    {
        let op: Token = tokens[*index];
        *index += 1;
        let expr: Expression = get_unary(tokens, logs, index);
        Expression::Unary { op, expr: Box::new(expr) }
    }
    else 
    {
        get_primary(tokens, logs, index)
    }
}

// Get a multiplicative expression (*, /).
fn get_multiplicative(tokens: &Vec<Token>, logs: &mut Vec<Log>, index: &mut usize) -> Expression
{
    let mut expr: Expression = get_unary(tokens, logs, index);
    while !expr.is_eof() &&
        (tokens[*index].token_type == TokenType::Star 
            || tokens[*index].token_type == TokenType::Slash
            || tokens[*index].token_type == TokenType::Percent)
    {
        let op: Token = tokens[*index];
        *index += 1;
        let right: Expression = get_unary(tokens, logs, index);
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
fn get_additive(tokens: &Vec<Token>, logs: &mut Vec<Log>, index: &mut usize) -> Expression
{
    let mut expr: Expression = get_multiplicative(tokens, logs, index);
    while !expr.is_eof() &&
        (tokens[*index].token_type == TokenType::Plus || tokens[*index].token_type == TokenType::Minus)
    {
        let op: Token = tokens[*index];
        *index += 1;
        let right: Expression = get_multiplicative(tokens, logs, index);
        let is_eof: bool = right.is_eof();
        expr = Expression::Binary{left: Box::new(expr), op, right: Box::new(right)};
        if is_eof
        {
            return expr;
        }
    }
    expr
} 

// Get a shift expression (<<, >>).
fn get_shift(tokens: &Vec<Token>, logs: &mut Vec<Log>, index: &mut usize) -> Expression
{
    let mut expr: Expression = get_additive(tokens, logs, index);
    while !expr.is_eof() &&
        (tokens[*index].token_type == TokenType::LeftShift 
            || tokens[*index].token_type == TokenType::RightShift)
    {
        let op: Token = tokens[*index];
        *index += 1;
        let right: Expression = get_additive(tokens, logs, index);
        let is_eof: bool = right.is_eof();
        expr = Expression::Binary{left: Box::new(expr), op, right: Box::new(right)};
        if is_eof
        {
            return expr;
        }
    }
    expr
} 

// Gets a bitwise and expression.
fn get_and(tokens: &Vec<Token>, logs: &mut Vec<Log>, index: &mut usize) -> Expression
{
    let mut expr: Expression = get_shift(tokens, logs, index);
    while !expr.is_eof() &&
        (tokens[*index].token_type == TokenType::Ampersand)
    {
        let op: Token = tokens[*index];
        *index += 1;
        let right: Expression = get_shift(tokens, logs, index);
        let is_eof: bool = right.is_eof();
        expr = Expression::Binary{left: Box::new(expr), op, right: Box::new(right)};
        if is_eof
        {
            return expr;
        }
    }
    expr
} 

// Gets a bitwise xor expression.
fn get_xor(tokens: &Vec<Token>, logs: &mut Vec<Log>, index: &mut usize) -> Expression
{
    let mut expr: Expression = get_and(tokens, logs, index);
    while !expr.is_eof() &&
        (tokens[*index].token_type == TokenType::Caret)
    {
        let op: Token = tokens[*index];
        *index += 1;
        let right: Expression = get_and(tokens, logs, index);
        let is_eof: bool = right.is_eof();
        expr = Expression::Binary{left: Box::new(expr), op, right: Box::new(right)};
        if is_eof
        {
            return expr;
        }
    }
    expr
} 

// Gets a bitwise or expression.
fn get_or(tokens: &Vec<Token>, logs: &mut Vec<Log>, index: &mut usize) -> Expression
{
    let mut expr: Expression = get_xor(tokens, logs, index);
    while !expr.is_eof() &&
        (tokens[*index].token_type == TokenType::Bar)
    {
        let op: Token = tokens[*index];
        *index += 1;
        let right: Expression = get_xor(tokens, logs, index);
        let is_eof: bool = right.is_eof();
        expr = Expression::Binary{left: Box::new(expr), op, right: Box::new(right)};
        if is_eof
        {
            return expr;
        }
    }
    expr
} 

// Simplify and correct the AST.
fn improve_ast(expr: Box<Expression>, parent: Option<Box<Expression>>, logs: &mut Vec<Log>)
{
    match *expr
    {
        Expression::Binary { ref left, op: _, ref right } =>
        {
            improve_ast(left.clone(), Some(expr.clone()), logs);
            improve_ast(right.clone(), Some(expr.clone()), logs);
        },
        Expression::Grouping { expr: ref child } =>
        {
            improve_ast(child.clone(), Some(expr.clone()), logs);
        },
        Expression::Literal { token } => 
        {
            if let TokenType::IntLiteral(0x8000_0000u32) = token.token_type
            {
                let mut preceded_by_unary: bool = false;
                if let Some(boxed_expr) = parent
                {
                    if let Expression::Unary { .. } = *boxed_expr
                    {
                        preceded_by_unary = true;
                    }
                }
                if !preceded_by_unary
                {
                    logs.push(Log{log_type: LogType::Error(ErrorType::UnnegatedMinimumIntegerLiteral),
                        line_and_col: Some((token.line, token.col))});
                }
            }
        },
        Expression::Unary { op: _, expr: ref child } =>
        {
            improve_ast(child.clone(), Some(expr.clone()), logs);
        },
        _ => { return; },
    }
}