//! The module for parsing the tokens and creating the AST.
use crate::{lexer, util::log};
use lexer::{LexerOutput, Token, TokenType};
use log::{ErrorType, Log, LogType};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};

/// The types in this language.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Unit, // Nothing type.
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Int => write!(f, "\"int\""),
            Self::Bool => write!(f, "\"bool\""),
            Self::Unit => write!(f, "\"()\""),
        }
    }
}

/// An enum represetning the possible types of expressions.
#[derive(Clone)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        op: Token,
        right: Box<Expression>,
        expr_type: Option<Type>,
    },
    ExpressionList {
        list: Vec<Box<Expression>>,
    },
    Grouping {
        expr: Box<Expression>,
        expr_type: Option<Type>,
    },
    Literal {
        token: Token,
        expr_type: Option<Type>,
    },
    Statement {
        expr: Box<Expression>,
    },
    Type {
        value: Type,
    },
    Unary {
        op: Token,
        expr: Box<Expression>,
        expr_type: Option<Type>,
    },
    Unit,
    Variable { 
        initialized: bool,
        token: Token,
        expr_type: Option<Type>,
    },
    VariableDeclaration {
        initialized_var: Box<Expression>,
    },

    EOF,
    Null,
}

/// The output given by the parser.
pub struct ParserOutput {
    pub file_text: String,
    pub expr: Expression,
    pub logs: Vec<Log>,
}

impl Expression {
    // Returns whether or not this expression represents the end of the file.
    fn is_eof(&self) -> bool {
        matches!(*self, Self::EOF)
    }

    // Returns the type of the expression
    #[must_use]
    pub fn get_type(&self) -> Option<Type> {
        match &self {
            Self::Binary { expr_type, .. }
            | Self::Grouping { expr_type, .. }
            | Self::Literal { expr_type, .. }
            | Self::Unary { expr_type, .. }
            | Self::Variable { expr_type, .. } => *expr_type,

            Self::ExpressionList { list } => match list.last() {
                None => Some(Type::Unit),
                Some(expr) => expr.get_type(),
            },

            Self::Statement { .. } | Self::Unit => Some(Type::Unit),

            Self::VariableDeclaration { initialized_var } => initialized_var.get_type(),

            Self::EOF | Self::Null | Self::Type{ .. } => None,
        }
    }
}

// Contains info about operators and the types they operate over
#[derive(Clone)]
struct Operator {
    token: TokenType,
    input: Vec<Type>,
    output: Type,
}

// Allows list of operators to have methods associated with them.
#[derive(Clone)]
struct OpList {
    list: Vec<Operator>,
}
impl OpList {
    // Gets the default list of operators used by the language.
    #[allow(clippy::too_many_lines)] // Decreasing lines only makes this function more dense.
    fn get_op_lists() -> [Self; 8] {
        [
            Self {
                list: vec![
                    Operator {
                        token: TokenType::Less,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Bool,
                    },
                    Operator {
                        token: TokenType::LessEqual,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Bool,
                    },
                    Operator {
                        token: TokenType::Greater,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Bool,
                    },
                    Operator {
                        token: TokenType::GreaterEqual,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Bool,
                    },
                    Operator {
                        token: TokenType::Equality,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Bool,
                    },
                    Operator {
                        token: TokenType::Equality,
                        input: vec![Type::Bool, Type::Bool],
                        output: Type::Bool,
                    },
                    Operator {
                        token: TokenType::Inequality,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Bool,
                    },
                    Operator {
                        token: TokenType::Inequality,
                        input: vec![Type::Bool, Type::Bool],
                        output: Type::Bool,
                    },
                ],
            },
            Self {
                list: vec![
                    Operator {
                        token: TokenType::Bar,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Int,
                    },
                    Operator {
                        token: TokenType::Bar,
                        input: vec![Type::Bool, Type::Bool],
                        output: Type::Bool,
                    },
                ],
            },
            Self {
                list: vec![
                    Operator {
                        token: TokenType::Caret,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Int,
                    },
                    Operator {
                        token: TokenType::Caret,
                        input: vec![Type::Bool, Type::Bool],
                        output: Type::Bool,
                    },
                ],
            },
            Self {
                list: vec![
                    Operator {
                        token: TokenType::Ampersand,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Int,
                    },
                    Operator {
                        token: TokenType::Ampersand,
                        input: vec![Type::Bool, Type::Bool],
                        output: Type::Bool,
                    },
                ],
            },
            Self {
                list: vec![
                    Operator {
                        token: TokenType::LeftShift,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Int,
                    },
                    Operator {
                        token: TokenType::RightShift,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Int,
                    },
                ],
            },
            Self {
                list: vec![
                    Operator {
                        token: TokenType::Plus,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Int,
                    },
                    Operator {
                        token: TokenType::Minus,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Int,
                    },
                ],
            },
            Self {
                list: vec![
                    Operator {
                        token: TokenType::Star,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Int,
                    },
                    Operator {
                        token: TokenType::Slash,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Int,
                    },
                    Operator {
                        token: TokenType::Percent,
                        input: vec![Type::Int, Type::Int],
                        output: Type::Int,
                    },
                ],
            },
            Self {
                list: vec![
                    Operator {
                        token: TokenType::Minus,
                        input: vec![Type::Int],
                        output: Type::Int,
                    },
                    Operator {
                        token: TokenType::Tilde,
                        input: vec![Type::Int],
                        output: Type::Int,
                    },
                    Operator {
                        token: TokenType::ExclamationMark,
                        input: vec![Type::Bool],
                        output: Type::Bool,
                    },
                ],
            },
        ]
    }

    // The number of arguments
    fn arg_count(&self) -> Option<usize> {
        if self.list.is_empty() {
            None
        } else {
            Some(self.list[0].input.len())
        }
    }

    // Returns whether or not one of the operators is associated a specific token.
    fn contains(&self, token: TokenType) -> bool {
        let list: Vec<Operator> = self.list.clone();
        for op in list {
            if op.token == token {
                return true;
            }
        }
        false
    }

    // Returns a list of valid input and output types for this
    fn valid_arg_types(&self, token: TokenType) -> Vec<(Vec<Type>, Type)> {
        let list: Vec<Operator> = self.list.clone();
        let mut valid_types: Vec<(Vec<Type>, Type)> = Vec::new();
        for op in list {
            if op.token == token {
                valid_types.push((op.input, op.output));
            }
        }
        valid_types
    }

    // Gets the output type of an operator given by the tokens and the given inputs.
    fn get_output_type(
        &self,
        token: &Token,
        input: Vec<Option<Type>>,
        log_info: &mut (&mut Vec<Log>, &String),
    ) -> Option<Type> {
        if input.contains(&None) {
            return None;
        }

        let input: Vec<Type> = {
            let mut temp: Vec<Type> = Vec::new();
            for value in input {
                temp.push(value.expect("checked by if statement"));
            }
            temp
        };

        let valid_types: Vec<(Vec<Type>, Type)> = self.valid_arg_types(token.token_type);
        let mut valid_inputs: Vec<Vec<Type>> = Vec::new();
        for value in valid_types.clone() {
            valid_inputs.push(value.clone().0);
        }
        for i in 0..valid_inputs.len() {
            if valid_inputs[i] == input {
                return Some(valid_types[i].1);
            }
        }

        let logs: &mut Vec<Log> = log_info.0;
        let source: &String = log_info.1;
        logs.push(Log {
            log_type: LogType::Error(ErrorType::InvalidArgsForOperator(
                token.to_string(source),
                {
                    let mut vec_string: Vec<String> = Vec::new();
                    for value in input {
                        vec_string.push(value.to_string());
                    }
                    vec_string
                },
            )),
            line_and_col: Some((token.line, token.col)),
        });
        None
    }
}

/// Parse the output from the lexer.
#[must_use]
pub fn parse(lex_output: LexerOutput) -> ParserOutput {
    let mut logs: Vec<Log> = lex_output.logs.clone();
    let mut index: usize = 0;
    let tokens: Vec<Token> = lex_output.tokens;
    let mut var_list: HashMap<String, Expression> = HashMap::new();
    let expr: Expression =
        get_expression_list(&tokens, &mut logs, &mut index, &lex_output.file_text, &mut var_list);
    if index < tokens.len() && tokens[index].token_type != TokenType::EOF {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::ExpectedEOF),
            line_and_col: Some((tokens[index].line, tokens[index].col)),
        });
    }
    improve_ast(Box::new(expr.clone()), None, &mut logs);
    ParserOutput {
        file_text: lex_output.file_text.clone(),
        expr,
        logs,
    }
}

// Gets a list of expressions from the token list. These should all be statements except for the last one.
fn get_expression_list(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
    var_list: &mut HashMap<String, Expression>
) -> Expression {
    let mut list: Vec<Box<Expression>> = Vec::new();
    let mut is_stmt: bool = true;
    while is_stmt {
        let next_expr: Expression = get_statement(tokens, logs, index, source, var_list);
        if let Expression::Statement { .. } = next_expr {
        } else {
            is_stmt = false;
        }
        list.push(Box::new(next_expr));
    }
    Expression::ExpressionList { list }
}

// Gets a statement from the token list.
fn get_statement(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
    var_list: &mut HashMap<String, Expression>
) -> Expression {
    if let TokenType::EOF = tokens[*index].token_type {
        return Expression::Unit;
    }

    let mut expr: Expression = get_expression(tokens, logs, index, source, var_list);
    if let TokenType::EOF = tokens[*index - 1].token_type {
        return expr;
    }

    if let TokenType::Semicolon = tokens[*index].token_type {
        expr = Expression::Statement {
            expr: Box::new(expr),
        };
        *index += 1;
    }
    expr
}

// Get the expression from the token list.
fn get_expression(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
    var_list: &mut HashMap<String, Expression>
) -> Expression {
    get_operators(tokens, logs, index, 0, source, var_list).unwrap_or(Expression::Null)
}

// Get a primary expression (literals and grouping expressions).
fn get_primary(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
    var_list: &mut HashMap<String, Expression>
) -> Expression {
    let token: Token = tokens[*index];
    *index += 1;
    match token.token_type {
        TokenType::IntLiteral(_) => Expression::Literal {
            token,
            expr_type: Some(Type::Int),
        },
        TokenType::True | TokenType::False => Expression::Literal {
            token,
            expr_type: Some(Type::Bool),
        },
        TokenType::Error => Expression::Literal {
            token,
            expr_type: None,
        },
        TokenType::LeftParen => handle_paren(tokens, logs, index, source, var_list),
        TokenType::EOF => {
            logs.push(Log {
                log_type: LogType::Error(ErrorType::UnexpectedEOF),
                line_and_col: Some((token.line, token.col)),
            });
            Expression::EOF
        }
        TokenType::Int => Expression::Type { value: Type::Int },
        TokenType::Bool => Expression::Type { value: Type::Bool },
        TokenType::Other => 
        {
            let key: &String = &token.to_string(source);
            if var_list.contains_key(key)
            {
                var_list[key].clone()
            } else {
                Expression::Variable { 
                    initialized: false, 
                    token, 
                    expr_type: None
                }
            }
        },
        _ => {
            logs.push(Log {
                log_type: LogType::Error(ErrorType::UnexpectedToken),
                line_and_col: Some((token.line, token.col)),
            });
            get_expression(tokens, logs, index, source, var_list)
        }
    }
}

// Handles primary expressions that use parentheses.
fn handle_paren(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
    var_list: &mut HashMap<String, Expression>
) -> Expression {
    if tokens[*index].token_type == TokenType::RightParen {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::ExpectedExpressionInParens),
            line_and_col: Some((tokens[*index].line, tokens[*index].col)),
        });
        *index += 1;
        return Expression::Grouping {
            expr: Box::new(Expression::Null),
            expr_type: None,
        };
    }
    let expr: Expression = get_expression(tokens, logs, index, source, var_list);
    if matches!(expr, Expression::EOF) {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::ExpectedCloseParen),
            line_and_col: Some((tokens[*index - 1].line, tokens[*index - 1].col)),
        });
        return expr;
    }

    let expr: Box<Expression> = Box::new(expr);
    if tokens[*index].token_type == TokenType::RightParen {
        *index += 1;
    } else {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::ExpectedCloseParen),
            line_and_col: Some((tokens[*index].line, tokens[*index].col)),
        });
    }
    let expr_type: Option<Type> = expr.get_type();
    Expression::Grouping { expr, expr_type }
}

// Gets an expression based on the operator precedence.
#[allow(clippy::redundant_else)] // TODO: Remove this later.
#[allow(clippy::question_mark)] // TODO: Remove this later.
fn get_operators(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    precendence: usize,
    source: &String,
    var_list: &mut HashMap<String, Expression>
) -> Option<Expression> {
    let operator_list: &[OpList] = &OpList::get_op_lists();
    // Variable setup
    if precendence == 0 {
        let old_index: usize = *index;
        let expr:Expression  = get_operators(tokens, logs, index, precendence + 1, source, var_list)?;
        if let Expression::Type{ value } = expr {
            let var: Option<Expression> = get_operators(tokens, logs, index, precendence, source, var_list);
            if let Some(var) = var {
                if let Expression::Variable { token, .. } = var {
                    let new_var: Expression = Expression::Variable { 
                        initialized: true,
                        token, 
                        expr_type: Some(value)
                    };
                    var_list.insert(token.to_string(source), new_var.clone());
                    return Some(new_var);
                }
            } else {
                logs.push(Log{
                    log_type: LogType::Error(ErrorType::ExpectedVariableDeclaration(value.to_string())),
                    line_and_col: Some((tokens[old_index].line, tokens[old_index].col))
                });
                return var;
            }
        }
        return Some(expr);
    }
    let precendence: usize = precendence - 1;

    // Other operators
    if precendence >= operator_list.len() {
        Some(get_primary(tokens, logs, index, source, var_list))
    } else if operator_list[precendence].arg_count()? == 1 {
        if operator_list[precendence].contains(tokens[*index].token_type) {
            let op: Token = tokens[*index];
            *index += 1;
            let expr: Expression = get_operators(tokens, logs, index, precendence, source, var_list)?;
            let expr_type: Option<Type> = operator_list[precendence].get_output_type(
                &op,
                vec![expr.get_type()],
                &mut (logs, source),
            );
            return Some(Expression::Unary {
                op,
                expr: Box::new(expr),
                expr_type,
            });
        }
        return get_operators(tokens, logs, index, precendence + 1, source, var_list);
    } else if operator_list[precendence].arg_count()? == 2 {
        let mut expr: Expression = get_operators(tokens, logs, index, precendence + 1, source, var_list)?;
        while !expr.is_eof() && operator_list[precendence].contains(tokens[*index].token_type) {
            let op: Token = tokens[*index];
            *index += 1;
            let right: Expression = get_operators(tokens, logs, index, precendence + 1, source, var_list)?;
            let type_list: Vec<Option<Type>> = vec![expr.get_type(), right.get_type()];
            let is_eof: bool = right.is_eof();
            expr = Expression::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
                expr_type: operator_list[precendence].get_output_type(
                    &op,
                    type_list,
                    &mut (logs, source),
                ),
            };
            if is_eof {
                return Some(expr);
            }
        }
        return Some(expr);
    } else {
        panic!("currently no other options for operators' argument counts.")
    }
}

// Simplify and correct the AST.
fn improve_ast(expr: Box<Expression>, parent: Option<Box<Expression>>, logs: &mut Vec<Log>) {
    match *expr {
        Expression::Binary {
            ref left,
            ref right,
            ..
        } => {
            improve_ast(left.clone(), Some(expr.clone()), logs);
            improve_ast(right.clone(), Some(expr), logs);
        }
        Expression::ExpressionList { ref list } => {
            for element in list {
                improve_ast(element.clone(), Some(expr.clone()), logs);
            }
        }
        Expression::Grouping {
            expr: ref child, ..
        }
        | Expression::Unary {
            expr: ref child, ..
        }
        | Expression::Statement { expr: ref child } => {
            improve_ast(child.clone(), Some(expr), logs);
        }
        Expression::Literal { token, .. } => {
            if token.token_type == TokenType::IntLiteral(0x8000_0000u32) {
                let mut preceded_by_unary: bool = false;
                if let Some(boxed_expr) = parent {
                    if let Expression::Unary { .. } = *boxed_expr {
                        preceded_by_unary = true;
                    }
                }
                if !preceded_by_unary {
                    logs.push(Log {
                        log_type: LogType::Error(ErrorType::UnnegatedMinimumIntegerLiteral),
                        line_and_col: Some((token.line, token.col)),
                    });
                }
            }
        }

        Expression::EOF | Expression::Null | Expression::Type{..} 
        | Expression::Unit | Expression::Variable { .. } | Expression::VariableDeclaration { .. }=> {}
    }
}
