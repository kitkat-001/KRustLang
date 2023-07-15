//! The module for parsing the tokens and creating the AST.
use crate::lexer;
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
    pub errors: Vec<String>,
    pub can_compile: bool,
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
    let mut can_compile: bool = lex_output.can_compile;
    let mut errors: Vec<String> = lex_output.errors.clone();
    let mut index: usize = 0;
    let tokens: Vec<Token> = lex_output.tokens;
    let expr: Expression = get_expression(&tokens.clone(), &mut errors, &mut can_compile, &mut index);
    if index < tokens.len() && tokens[index].token_type != TokenType::EOF
    {
        errors.push(format!("error (line {}:{}): expected EOF", tokens[index].line, tokens[index].col));
        can_compile = false;
    }
    improve_ast(Box::new(expr.clone()), None, &mut errors, &mut can_compile);
    ParserOutput{file_text: lex_output.file_text.clone(), expr, errors, can_compile}
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

// Simplify and correct the AST.
fn improve_ast(expr: Box<Expression>, parent: Option<Box<Expression>>, errors: &mut Vec<String>, can_compile: &mut bool)
{
    match *expr
    {
        Expression::Binary { ref left, op: _, ref right } =>
        {
            improve_ast(left.clone(), Some(expr.clone()), errors, can_compile);
            improve_ast(right.clone(), Some(expr.clone()), errors, can_compile);
        },
        Expression::Grouping { expr: ref child } =>
        {
            improve_ast(child.clone(), Some(expr.clone()), errors, can_compile);
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
                    errors.push(format!("error (line {}:{}): the int literal {} must be preceded by a unary \'-\' operator",
                        token.line, token.col, 0x8000_0000u32));
                    *can_compile = false;
                }
            }
        },
        Expression::Unary { op: _, expr: ref child } =>
        {
            improve_ast(child.clone(), Some(expr.clone()), errors, can_compile);
        },
        _ => { return; },
    }
}