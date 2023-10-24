//! The module for parsing the tokens and creating the AST.
use crate::{lexer, util::log};
use lexer::{LexerOutput, Token, TokenType};
use log::{ErrorType, InfoType, Log, LogType};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};

/// The types in this language.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Type {
    Byte,
    Int,
    Bool,
    Void, // Nothing type.
    Type,
}

impl Type {
    /// Get the types that this type can be casted to.
    fn valid_casts(&self) -> Vec<Self> {
        match self {
            Self::Byte | Self::Int | Self::Bool => vec![Self::Byte, Self::Int, Self::Bool],
            Self::Void => vec![Self::Void],
            Self::Type => vec![Self::Type],
        }
    }

    fn is_num_type(&self) -> bool {
        [Self::Byte, Self::Int].contains(self)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Byte => write!(f, "\"byte\""),
            Self::Int => write!(f, "\"int\""),
            Self::Bool => write!(f, "\"bool\""),
            Self::Void => write!(f, "\"void\""),
            Self::Type => write!(f, "\"type\""),
        }
    }
}

/// An enum represetning the possible types of expressions.
#[derive(Clone, Debug)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        op: Token,
        right: Box<Expression>,
        expr_type: Option<Type>,
    },
    Cast {
        expr_type: Option<Type>,
        expr: Box<Expression>,
    },
    CastOp {
        expr_type: Type,
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
    Void,
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
            | Self::Cast { expr_type, .. }
            | Self::Grouping { expr_type, .. }
            | Self::Literal { expr_type, .. }
            | Self::Unary { expr_type, .. }
            | Self::Variable { expr_type, .. } => *expr_type,

            Self::CastOp { expr_type } => Some(*expr_type),

            Self::ExpressionList { list } => match list.last() {
                None => Some(Type::Void),
                Some(expr) => expr.get_type(),
            },

            Self::Statement { .. } | Self::Void => Some(Type::Void),

            Self::Type { .. } => Some(Type::Type),

            Self::VariableDeclaration { initialized_var } => initialized_var.get_type(),

            Self::EOF | Self::Null => None,
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

// Macro for creating operators that act on all numerical types.
macro_rules! num_ops {
    ($op_list: expr) => ($op_list);
    ($op_list: expr, $op: expr, $output_type: expr; 1) => {
        {
            let list: Vec<Operator> = $op_list.into_iter().chain(vec![Operator {
                token: $op,
                input: vec![Type::Byte],
                output: $output_type.unwrap_or(Type::Byte),
            },
            Operator {
                token: $op,
                input: vec![Type::Int],
                output: $output_type.unwrap_or(Type::Int)
            }].into_iter()).collect();
            list
        }
    };
    ($op_list: expr, $op: expr, $output_type: expr, $($extra_ops: expr, $extra_outputs: expr), +; 1) => {
        {
            let list: Vec<Operator> = num_ops!($op_list, $($extra_ops, $extra_outputs), +; 1).into_iter().chain(vec![Operator {
                token: $op,
                input: vec![Type::Byte],
                output: $output_type.unwrap_or(Type::Byte),
            },
            Operator {
                token: $op,
                input: vec![Type::Int],
                output: $output_type.unwrap_or(Type::Int),
            }].into_iter()).collect();
            list
        }
    };
    ($op_list: expr, $op: expr, $output_type: expr; 2) => {
        {
            let list: Vec<Operator> = $op_list.into_iter().chain(vec![Operator {
                token: $op,
                input: vec![Type::Byte, Type::Byte],
                output: $output_type.unwrap_or(Type::Byte),
            },
            Operator {
                token: $op,
                input: vec![Type::Int, Type::Int],
                output: $output_type.unwrap_or(Type::Int)
            }].into_iter()).collect();
            list
        }
    };
    ($op_list: expr, $op: expr, $output_type: expr, $($extra_ops: expr, $extra_outputs: expr), +; 2) => {
        {
            let list: Vec<Operator> = num_ops!($op_list, $($extra_ops, $extra_outputs), +; 2).into_iter().chain(vec![Operator {
                token: $op,
                input: vec![Type::Byte, Type::Byte],
                output: $output_type.unwrap_or(Type::Byte),
            },
            Operator {
                token: $op,
                input: vec![Type::Int, Type::Int],
                output: $output_type.unwrap_or(Type::Int),
            }].into_iter()).collect();
            list
        }
    };
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
                list: num_ops!(vec![
                    Operator {
                        token: TokenType::Equality,
                        input: vec![Type::Bool, Type::Bool],
                        output: Type::Bool,
                    },
                    Operator {
                        token: TokenType::Inequality,
                        input: vec![Type::Bool, Type::Bool],
                        output: Type::Bool,
                    },
                ], TokenType::Less, Some(Type::Bool), TokenType::LessEqual, Some(Type::Bool),
                TokenType::Greater, Some(Type::Bool), TokenType::GreaterEqual, Some(Type::Bool),
                TokenType::Equality, Some(Type::Bool), TokenType::Inequality, Some(Type::Bool); 2),
            },
            Self {
                list: num_ops!(vec![
                    Operator {
                        token: TokenType::Bar,
                        input: vec![Type::Bool, Type::Bool],
                        output: Type::Bool,
                    },
                ], TokenType::Bar, None; 2),
            },
            Self {
                list: num_ops!(vec![
                    Operator {
                        token: TokenType::Caret,
                        input: vec![Type::Bool, Type::Bool],
                        output: Type::Bool,
                    },
                ], TokenType::Caret, None; 2),
            },
            Self {
                list: num_ops!(vec![
                    Operator {
                        token: TokenType::Ampersand,
                        input: vec![Type::Bool, Type::Bool],
                        output: Type::Bool,
                    },
                ], TokenType::Ampersand, None; 2),
            },
            Self {
                list: num_ops!(Vec::new(),
                TokenType::LeftShift, None, TokenType::RightShift, None; 2),
            },
            Self {
                list: num_ops!(Vec::new(),
                TokenType::Plus, None, TokenType::Minus, None; 2),
            },
            Self {
                list: num_ops!(Vec::new(),
                TokenType::Star, None, TokenType::Slash, None, TokenType::Percent, None; 2),
            },
            Self {
                list: num_ops!(vec![
                    Operator {
                        token: TokenType::ExclamationMark,
                        input: vec![Type::Bool],
                        output: Type::Bool,
                    },
                ], TokenType::Minus, None, TokenType::Tilde, None; 1),
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
    let expr: Expression = get_expression_list(
        &tokens,
        &mut logs,
        &mut index,
        &lex_output.file_text,
        &mut var_list,
    );
    if index < tokens.len() && tokens[index].token_type != TokenType::EOF {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::UnexpectedToken(
                tokens[index].to_string(&lex_output.file_text),
            )),
            line_and_col: Some((tokens[index].line, tokens[index].col)),
        });
    }
    improve_ast(
        Box::new(expr.clone()),
        None,
        &mut logs,
        &lex_output.file_text,
    );
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
    var_list: &mut HashMap<String, Expression>,
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
    var_list: &mut HashMap<String, Expression>,
) -> Expression {
    if let TokenType::EOF = tokens[*index].token_type {
        return Expression::Void;
    }

    let mut expr: Expression = get_expression(tokens, logs, index, source, var_list);
    if let Expression::VariableDeclaration { initialized_var } = &expr {
        if let Expression::Variable { token, .. } = **initialized_var {
            var_list.insert(token.to_string(source), *initialized_var.clone());
        }
    }

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
    var_list: &mut HashMap<String, Expression>,
) -> Expression {
    handle_assignment(tokens, logs, index, source, var_list).unwrap_or(Expression::Null)
}

// Get a primary expression (literals and grouping expressions).
fn get_primary(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
    var_list: &mut HashMap<String, Expression>,
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
        //TokenType::Byte => Expression::Type { value: Type::Byte },
        TokenType::Int => Expression::Type { value: Type::Int },
        TokenType::Bool => Expression::Type { value: Type::Bool },
        TokenType::Other => {
            let key: &String = &token.to_string(source);
            if var_list.contains_key(key) {
                var_list[key].clone()
            } else {
                Expression::Variable {
                    initialized: false,
                    token,
                    expr_type: None,
                }
            }
        }
        _ => {
            logs.push(Log {
                log_type: LogType::Error(ErrorType::UnexpectedToken(token.to_string(source))),
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
    var_list: &mut HashMap<String, Expression>,
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

    if tokens[*index].token_type == TokenType::RightParen {
        *index += 1;
    } else {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::ExpectedCloseParen),
            line_and_col: Some((tokens[*index].line, tokens[*index].col)),
        });
    }
    if let Expression::Type { value } = expr {
        Expression::CastOp { expr_type: value }
    } else {
        let expr: Box<Expression> = Box::new(expr);
        let expr_type: Option<Type> = expr.get_type();
        Expression::Grouping { expr, expr_type }
    }
}

// Handles variable assignment.
fn handle_assignment(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
    var_list: &mut HashMap<String, Expression>,
) -> Option<Expression> {
    let mut expr: Expression = get_variable_declaration(tokens, logs, index, source, var_list)?;
    let mut var: Expression = expr.clone();
    let mut is_declaration: bool = false;
    if let Expression::VariableDeclaration { initialized_var } = var {
        var = *initialized_var;
        is_declaration = true;
    }
    if let Expression::Variable {
        token, expr_type, ..
    } = var
    {
        let op = tokens[*index];
        if let TokenType::Equals = tokens[*index].token_type {
            *index += 1;
            let mut expr_type: Option<Type> = expr_type;
            let mut assignment: Expression = get_expression(tokens, logs, index, source, var_list);
            if assignment.get_type() != expr_type {
                if assignment.get_type().is_some() && expr_type.is_some() {
                    // AUto-casting for int literals.
                    if let Expression::Literal { token, .. } = assignment {
                        if let TokenType::IntLiteral(value) = token.token_type {
                            if in_range(value, expr_type) {
                                assignment = Expression::Cast {
                                    expr_type,
                                    expr: Box::new(assignment),
                                };
                            }
                        }
                    } else {
                        logs.push(Log {
                            log_type: LogType::Error(ErrorType::InvalidArgsForAssignment(
                                token.to_string(source),
                                [expr_type?.to_string(), assignment.get_type()?.to_string()], // Both types are not null here.
                            )),
                            line_and_col: Some((op.line, op.col)),
                        });
                    }
                }
                expr_type = None;
            }
            expr = Expression::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(assignment),
                expr_type,
            };
            if is_declaration {
                var_list.insert(token.to_string(source), var.clone());
            }
        }
    }

    Some(expr)
}

// Determines if a numerical value can fit in the given type.
fn in_range(value: u32, expr_type: Option<Type>) -> bool {
    match expr_type {
        Some(Type::Int) => true,
        Some(Type::Byte) => value <= u8::MAX.into(),
        _ => false,
    }
}

// Handle variable declarations.
fn get_variable_declaration(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
    var_list: &mut HashMap<String, Expression>,
) -> Option<Expression> {
    let old_index: usize = *index;
    let expr: Expression = get_operators(tokens, logs, index, 0, source, var_list)?;
    if let Expression::Type { value } = expr {
        if tokens[*index].token_type == TokenType::RightParen {
            return Some(expr);
        }
        let var: Option<Expression> = get_operators(tokens, logs, index, 0, source, var_list);
        if let Some(var) = var {
            if let Expression::Variable { token, .. } = var {
                let new_var: Expression = Expression::Variable {
                    initialized: true,
                    token,
                    expr_type: Some(value),
                };
                return Some(Expression::VariableDeclaration {
                    initialized_var: Box::new(new_var),
                });
            }
        } else {
            logs.push(Log {
                log_type: LogType::Error(ErrorType::ExpectedVariableDeclaration(value.to_string())),
                line_and_col: Some((tokens[old_index].line, tokens[old_index].col)),
            });
            return var;
        }
    }
    Some(expr)
}

// Gets an expression based on the operator precedence.
fn get_operators(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    precendence: usize,
    source: &String,
    var_list: &mut HashMap<String, Expression>,
) -> Option<Expression> {
    let operator_list: &[OpList] = &OpList::get_op_lists();
    if precendence >= operator_list.len() {
        get_cast(tokens, logs, index, source, var_list)
    } else if operator_list[precendence].arg_count()? == 1 {
        if operator_list[precendence].contains(tokens[*index].token_type) {
            let op: Token = tokens[*index];
            *index += 1;
            let expr: Expression =
                get_operators(tokens, logs, index, precendence, source, var_list)?;
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
        let mut expr: Expression =
            get_operators(tokens, logs, index, precendence + 1, source, var_list)?;
        if let Expression::CastOp { .. } = expr {
            return Some(expr);
        }
        while !expr.is_eof() && operator_list[precendence].contains(tokens[*index].token_type) {
            let op: Token = tokens[*index];
            *index += 1;
            let right: Expression =
                get_operators(tokens, logs, index, precendence + 1, source, var_list)?;
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

// Handle casts.
fn get_cast(
    tokens: &Vec<Token>,
    logs: &mut Vec<Log>,
    index: &mut usize,
    source: &String,
    var_list: &mut HashMap<String, Expression>,
) -> Option<Expression> {
    let old_index: usize = *index;
    let expr: Expression = get_primary(tokens, logs, index, source, var_list);
    if let Expression::CastOp { expr_type } = expr {
        let right: Option<Expression> = get_operators(
            tokens,
            logs,
            index,
            OpList::get_op_lists().len() - 1,
            source,
            var_list,
        ); // Check for unary operations first.
        if let Some(right) = right {
            Some(Expression::Cast {
                expr_type: if right.get_type().is_some()
                    && expr_type
                        .valid_casts()
                        .contains(&right.get_type().expect("checked by if"))
                {
                    Some(expr_type)
                } else {
                    logs.push(Log {
                        log_type: LogType::Error(ErrorType::InvalidTypesForCast(
                            expr_type.to_string(),
                            match right.get_type() {
                                Option::Some(t) => t.to_string(),
                                Option::None => "none".to_string(),
                            },
                        )),
                        line_and_col: Some((tokens[old_index].line, tokens[old_index].col)),
                    });
                    None
                },
                expr: Box::new(right),
            })
        } else {
            logs.push(Log {
                log_type: LogType::Error(ErrorType::ExpectedExpressionAfterCast(
                    expr_type.to_string(),
                )),
                line_and_col: Some((tokens[old_index].line, tokens[old_index].col)),
            });
            right
        }
    } else {
        Some(expr)
    }
}

// Simplify and correct the AST.
fn improve_ast(
    expr: Box<Expression>,
    parent: Option<Box<Expression>>,
    logs: &mut Vec<Log>,
    source: &String,
) {
    match *expr {
        Expression::Binary {
            ref left,
            ref right,
            ..
        } => {
            improve_ast(left.clone(), Some(expr.clone()), logs, source);
            improve_ast(right.clone(), Some(expr), logs, source);
        }
        Expression::ExpressionList { ref list } => {
            for element in list {
                improve_ast(element.clone(), Some(expr.clone()), logs, source);
            }
        }
        Expression::Grouping {
            expr: ref child, ..
        }
        | Expression::Unary {
            expr: ref child, ..
        }
        | Expression::Statement { expr: ref child } => {
            improve_ast(child.clone(), Some(expr), logs, source);
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
        Expression::Variable {
            initialized, token, ..
        } => {
            if !initialized {
                if let Some(parent) = parent {
                    if let Expression::VariableDeclaration { .. } = *parent {
                        return;
                    }
                }
                logs.push(Log {
                    log_type: LogType::Error(ErrorType::ExpectedVariableDeclaration(
                        token.to_string(source),
                    )),
                    line_and_col: Some((token.line, token.col)),
                });
            }
        }
        Expression::VariableDeclaration { initialized_var } => {
            if let Some(parent) = parent {
                if let Expression::Binary { op, .. } = *parent {
                    if let TokenType::Equals = op.token_type {
                        return;
                    }
                }
            }

            // Always true.
            if let Expression::Variable { token, .. } = *initialized_var {
                logs.push(Log {
                    log_type: LogType::Info(InfoType::NewVarNotSet(token.to_string(source))),
                    line_and_col: Some((token.line, token.col)),
                });
            }
        }

        Expression::Cast { .. }
        | Expression::CastOp { .. }
        | Expression::EOF
        | Expression::Null
        | Expression::Type { .. }
        | Expression::Void => {}
    }
}
