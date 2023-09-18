//! The module for parsing the tokens and creating the AST.
use crate::{lexer, log};
use lexer::{LexerOutput, Token, TokenType};
use log::{ErrorType, Log, LogType};
use std::fmt::{Display, Formatter, Result};

/// The types in this language.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Int => write!(f, "\"int\""),
            Self::Bool => write!(f, "\"bool\""),
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
    Grouping {
        expr: Box<Expression>,
        expr_type: Option<Type>,
    },
    Literal {
        token: Token,
        expr_type: Option<Type>,
    },
    Unary {
        op: Token,
        expr: Box<Expression>,
        expr_type: Option<Type>,
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
        if matches!(*self, Self::EOF) {
            true
        } else {
            false
        }
    }

    // Returns the type of the expression
    #[must_use]
    pub fn get_type(&self) -> Option<Type> {
        match &self {
            Self::Binary { expr_type, .. } => *expr_type,
            Self::Grouping { expr_type, .. } => *expr_type,
            Self::Literal { expr_type, .. } => *expr_type,
            Self::Unary { expr_type, .. } => *expr_type,

            _ => None,
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
    fn get_op_lists() -> [Self; 7] {
        return [
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
        if !self.list.is_empty() {
            Some(self.list[0].input.len())
        } else {
            None
        }
    }

    // Returns whether or not one of the operators is associated a specific token.
    fn contains(&self, token: &TokenType) -> bool {
        let list: Vec<Operator> = self.list.clone();
        for op in list {
            if op.token == *token {
                return true;
            }
        }
        false
    }

    // Returns a list of valid input and output types for this
    fn valid_arg_types(&self, token: &TokenType) -> Vec<(Vec<Type>, Type)> {
        let list: Vec<Operator> = self.list.clone();
        let mut valid_types: Vec<(Vec<Type>, Type)> = Vec::new();
        for op in list {
            if op.token == *token {
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
        log_info: (&mut Vec<Log>, &String),
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

        let valid_types: Vec<(Vec<Type>, Type)> = self.valid_arg_types(&token.token_type);
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
    let expr: Expression = get_expression(&tokens, &mut logs, &mut index, &lex_output.file_text);
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

// Get the expression from the token list.
fn get_expression(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
) -> Expression {
    get_operators(tokens, logs, index, 0, source).unwrap_or(Expression::Null)
}

// Get a primary expression (literals and grouping expressions).
fn get_primary(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
) -> Expression {
    let token: Token = tokens[*index];
    *index += 1;
    match token.token_type {
        TokenType::IntLiteral(_) => Expression::Literal {
            token,
            expr_type: Some(Type::Int),
        },
        TokenType::True => Expression::Literal {
            token,
            expr_type: Some(Type::Bool),
        },
        TokenType::False => Expression::Literal {
            token,
            expr_type: Some(Type::Bool),
        },
        TokenType::Error => Expression::Literal {
            token,
            expr_type: None,
        },
        TokenType::LeftParen => handle_paren(tokens, logs, index, source),
        TokenType::EOF => {
            logs.push(Log {
                log_type: LogType::Error(ErrorType::UnexpectedEOF),
                line_and_col: Some((token.line, token.col)),
            });
            Expression::EOF
        }
        _ => {
            logs.push(Log {
                log_type: LogType::Error(ErrorType::UnexpectedToken),
                line_and_col: Some((token.line, token.col)),
            });
            get_expression(tokens, logs, index, source)
        }
    }
}

// Handles primary expressions that use parentheses.
fn handle_paren(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
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
    let expr: Expression = get_expression(tokens, logs, index, source);
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
fn get_operators(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    precendence: usize,
    source: &String,
) -> Option<Expression> {
    let operator_list: [OpList; 7] = OpList::get_op_lists();
    if precendence >= operator_list.len() {
        Some(get_primary(tokens, logs, index, source))
    } else if operator_list[precendence].arg_count()? == 1 {
        if operator_list[precendence].contains(&tokens[*index].token_type) {
            let op: Token = tokens[*index];
            *index += 1;
            let expr: Expression = get_operators(tokens, logs, index, precendence, source)?;
            let expr_type: Option<Type> = operator_list[precendence].get_output_type(
                &op,
                vec![expr.get_type()],
                (logs, source),
            );
            return Some(Expression::Unary {
                op,
                expr: Box::new(expr),
                expr_type,
            });
        } else {
            return get_operators(tokens, logs, index, precendence + 1, source);
        }
    } else if operator_list[precendence].arg_count()? == 2 {
        let mut expr: Expression = get_operators(tokens, logs, index, precendence + 1, source)?;
        while !expr.is_eof() && operator_list[precendence].contains(&tokens[*index].token_type) {
            let op: Token = tokens[*index];
            *index += 1;
            let right: Expression = get_operators(tokens, logs, index, precendence + 1, source)?;
            let type_list: Vec<Option<Type>> = vec![expr.get_type(), right.get_type()];
            let is_eof: bool = right.is_eof();
            expr = Expression::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
                expr_type: operator_list[precendence].get_output_type(
                    &op,
                    type_list,
                    (logs, source),
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
        Expression::Grouping {
            expr: ref child, ..
        } => {
            improve_ast(child.clone(), Some(expr), logs);
        }
        Expression::Literal { token, .. } => {
            if token.token_type == TokenType::IntLiteral(0x8000_0000_u32) {
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
        Expression::Unary {
            expr: ref child, ..
        } => {
            improve_ast(child.clone(), Some(expr), logs);
        }
        _ => {}
    }
}
