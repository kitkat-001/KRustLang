//! The module for the virtual machine used by the language.

pub mod math;

use crate::{log, compiler};
use log::{Log, LogType, ErrorType, is_error};
use compiler::OpCode;
use math::shift_int;

use num_traits::FromPrimitive;

// Contains info about a runtime error that could happen.
struct RuntimeError<'a, T>
{
    condition: &'a (dyn Fn(T) -> bool),
    error: ErrorType,
    index: &'a mut usize,
    bytecode: &'a Vec<u8>,
}

/// Runs the bytecode.
pub fn run(bytecode: &Vec<u8>) -> (Vec<String>, Vec<Log>)
{
    let mut output: Vec<String> = Vec::new();
    let mut logs: Vec<Log> = Vec::new();

    let possible_error = handle_errors(bytecode, &mut output, &mut logs);
    let possible_error: Option<(&Vec<String>, &Vec<Log>)> = possible_error.0;
    if possible_error.is_some()
    {
        let error: (&Vec<String>, &Vec<Log>) = possible_error.expect("if statement");
        return (error.0.clone(), error.1.clone());
    }

    let mut index: usize = 2;
    let mut stack: Vec<u8> = Vec::new();
    while index < bytecode.len()
    {
        let curr_op: Option<OpCode> = FromPrimitive::from_u8(bytecode[index]);
        index += 1;

        match curr_op
        {
            Some(op) => 
            {
                if match_op(op, bytecode, &mut stack, &mut index, &mut output, &mut logs)
                {
                    return (output, logs);
                }
            }
            None => 
            { 
                logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
                return (output, logs);
            }
        }
    }
    (output, logs)
}

// Handle any errors immediatly present in the bytecode.
fn handle_errors<'o, 'e>(bytecode: &Vec<u8>, output: &'o mut Vec<String>, logs: &'e mut Vec<Log>) 
    -> (Option<(&'o Vec<String>, &'e Vec<Log>)>, Option<(usize, bool)>)
{
    if bytecode.len() < 2
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
        return (Some((output, logs)), None);
    }
    let ptr_size: usize = bytecode[0] as usize;
    let detailed_err: bool = bytecode[1] != 0;
    if ptr_size * 8 > usize::BITS.try_into().expect("max value of usize must be less than the number of bits")
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::CompiledForDifferentTarget(ptr_size * 8)), line_and_col: None});
        return (Some((output, logs)), Some((ptr_size, detailed_err)));
    }
    (None, Some((ptr_size, detailed_err)))
}

// Runs a function given a specific op code.
fn match_op(
    op: OpCode, 
    bytecode: &Vec<u8>, 
    stack: &mut Vec<u8>,
    index: &mut usize,
    output: &mut Vec<String>,
    logs: &mut Vec<Log>,
) -> bool
{
    match op
    {
        OpCode::PushInt => push_int(bytecode, stack, index, logs),
        OpCode::PushByte => push_byte(bytecode, stack, index, logs),
        OpCode::PopInt => pop_int(stack, output, logs),
        OpCode::PopBool => pop_bool(stack, output, logs),
        OpCode::MinusInt => minus_int(stack, logs),
        OpCode::AddInt => add_int(stack, logs),
        OpCode::SubtractInt => subtract_int(stack, logs),
        OpCode::MultiplyInt => multiply_int(stack, logs),
        OpCode::DivideInt => divide_int(bytecode, stack, index, logs),
        OpCode::ModuloInt => modulo_int(bytecode, stack, index, logs),
        OpCode::ComplementInt => complement_int(stack, logs),
        OpCode::LeftShiftInt => left_shift_int(stack, logs),
        OpCode::RightShiftInt => right_shift_int(stack, logs),
        OpCode::AndInt => and_int(stack, logs),
        OpCode::AndBool => and_bool(stack, logs),
        OpCode::XorInt => xor_int(stack, logs),
        OpCode::XorBool => xor_bool(stack, logs),
        OpCode::OrInt => or_int(stack, logs),
        OpCode::OrByte => or_byte(stack, logs)
    };
    is_error(logs)
}

// Pushes an int from the bytecode to the stack.
fn push_int(bytecode: &Vec<u8>, stack: &mut Vec<u8>, index: &mut usize, logs: &mut Vec<Log>)
{
    if *index + 4 > bytecode.len()
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
    for _i in 0..4
    {
        stack.push(bytecode[*index]);
        *index += 1;
    }
}

// Pushes a byte onto the stack.
fn push_byte(bytecode: &Vec<u8>, stack: &mut Vec<u8>, index: &mut usize, logs: &mut Vec<Log>)
{
    if *index + 1 > bytecode.len()
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
    stack.push(bytecode[*index]);
    *index += 1;
}

// Pops an int from the stack and adds it to the output.
fn pop_int(stack: &mut Vec<u8>, output: &mut Vec<String>, logs: &mut Vec<Log>)
{
    let value: Option<i32> = pop_int_from_stack(stack);
    if let Some(value) = value
    {
        output.push(format!("{}", value));
    }
    else
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Pops an bool from the stack and adds it to the output.
fn pop_bool(stack: &mut Vec<u8>, output: &mut Vec<String>, logs: &mut Vec<Log>)
{
    let value: Option<u8> = stack.pop();
    if let Some(value) = value
    {
        output.push(format!("{}", value != 0));
    }
    else
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Gets the negative of an int.
fn minus_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    unary_int(stack, logs, |a: i32| i32::wrapping_neg(a));
}

// Adds two ints.
fn add_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    binary_int(stack, logs, |a, b| i32::wrapping_add(a, b), None);
}

// Subtracts two ints.
fn subtract_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)

{
    binary_int(stack, logs, |a, b| i32::wrapping_sub(a, b), None);
}

// Multiplies two ints.
fn multiply_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>) 
{
    binary_int(stack, logs, |a, b| i32::wrapping_mul(a, b), None);
}

// Divides two ints. Reports an error if the second int is zero.
fn divide_int(bytecode: &Vec<u8>,  stack: &mut Vec<u8>, index: &mut usize, logs: &mut Vec<Log>)
{
    binary_int(stack, logs,  |a, b| if b == 0 {0} else {i32::wrapping_div(a, b)}, 
        Some(RuntimeError::<(i32, i32)>{
            condition: &(|(_a, b)| b == 0),
            error: ErrorType::DivideByZero,
            index, bytecode
    }))
}

// Gets the modulo of two ints. Reports an error if the second int is zero.
fn modulo_int(bytecode: &Vec<u8>,  stack: &mut Vec<u8>, index: &mut usize, logs: &mut Vec<Log>)
{
    binary_int(stack, logs,  |a, b| if b == 0 {0} else {i32::wrapping_rem_euclid(a, b)}, 
        Some(RuntimeError::<(i32, i32)>{
            condition: &(|(_a, b)| b == 0),
            error: ErrorType::DivideByZero,
            index, bytecode
    }))
}

// Finds the complement of an int.
fn complement_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    unary_int(stack, logs, |a: i32| !a);
}


// Left shifts an int by an int
fn left_shift_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    binary_int(stack, logs, |a, b| shift_int(a, b), None);
}

// Left shifts an int by an int
fn right_shift_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    binary_int(stack, logs, |a, b| shift_int(a, -b), None);
}

// Bitwise ands two ints.
fn and_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    binary_int(stack, logs, |a, b| a & b, None);
}

// Ands two booleans.
fn and_bool(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    binary_byte(stack, logs, |a, b| (a != 0 && b != 0) as u8, None);
}

// Bitwise xors two ints.
fn xor_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    binary_int(stack, logs, |a, b| a ^ b, None);
}

// Xors two booleans.
fn xor_bool(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    binary_byte(stack, logs, |a, b| ((a != 0) != (b != 0)) as u8, None);
}

// Bitwise ors two ints.
fn or_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    binary_int(stack, logs, |a, b| a | b, None);
}

// Bitwise ors two bytes.
fn or_byte(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    binary_byte(stack, logs, |a, b| a | b, None);
}

// Performs a unary operation on an int.
fn unary_int<F>(stack: &mut Vec<u8>, logs: &mut Vec<Log>, func: F)
    where F: Fn(i32) -> i32
{
    let value: Option<i32> = pop_int_from_stack(stack);
    if let Some(value) = value
    {
        let value: i32 = func(value);
        stack.append(&mut value.to_le_bytes().to_vec());
    }
    else 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Performs a binary operation on a pair of ints.
fn binary_int<F>(stack: &mut Vec<u8>, logs: &mut Vec<Log>, func: F, error: Option<RuntimeError<(i32, i32)>>)
    where F: Fn(i32, i32) -> i32
{
    let detailed_err: bool = if let Some(error) = &error
    {
        get_detailed_err(error.bytecode)
    }
    else
    {
        false
    }; 
    if detailed_err && errors_stored_incorrectly(error.as_ref().expect("detailed_err is true"))
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
        return;
    }

    let b: Option<i32> = pop_int_from_stack(stack);
    let a: Option<i32> = pop_int_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            if let Some(mut error) = error
            {
                handle_error(&mut error, (a, b), detailed_err, logs)
            }
            let c: i32 = func(a, b);
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Performs a binary operation on a pair of bytes.
fn binary_byte<F>(stack: &mut Vec<u8>, logs: &mut Vec<Log>, func: F, error: Option<RuntimeError<(u8, u8)>>)
    where F: Fn(u8, u8) -> u8
{
    let detailed_err: bool = if let Some(error) = &error
    {
        get_detailed_err(error.bytecode)
    }
    else
    {
        false
    }; 
    if detailed_err && errors_stored_incorrectly(error.as_ref().expect("detailed_err is true"))
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
        return;
    }

    let b: Option<u8> = stack.pop();
    let a: Option<u8> = stack.pop();
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            if let Some(mut error) = error
            {
                handle_error(&mut error, (a, b), detailed_err, logs)
            }
            let c: u8 = func(a, b);
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Checks to make sure that line info is properly stored in operations that may have an error.
fn errors_stored_incorrectly<T>(error: &RuntimeError<T>) -> bool
{
    let index: usize = *error.index;
    let bytecode: &Vec<u8> = error.bytecode;
    index + 2 * get_ptr_size(bytecode) > bytecode.len()
}

// Handles runtime errors.
fn handle_error<T>(error: &mut RuntimeError<T>, value: T, detailed_err: bool, logs: &mut Vec<Log>)
{
    let condition: &dyn Fn(T) -> bool = error.condition;
    let index: &mut usize = error.index;
    let bytecode: &Vec<u8> = error.bytecode;
    let ptr_size: usize = get_ptr_size(bytecode);
    if condition(value)
    {
        if detailed_err
        {
            let mut bytes : [u8; (usize::BITS / 8) as usize] = [0; (usize::BITS / 8) as usize];
            for i in 0..ptr_size
            {
                bytes[i] = bytecode[*index];
                *index += 1;
            }
            let line: usize = usize::from_le_bytes(bytes);
            bytes = [0; (usize::BITS / 8) as usize];
            for i in 0..ptr_size
            {
                bytes[i] = bytecode[*index];
                *index += 1;
            }
            let col: usize = usize::from_le_bytes(bytes);
            logs.push(Log{log_type: LogType::Error(error.error.clone()), line_and_col: Some((line, col))});
        }
        else 
        {
            logs.push(Log{log_type: LogType::Error(error.error.clone()), line_and_col: None});
        }
    }
    else if detailed_err
    {
        *index += 2 * get_ptr_size(bytecode);
    }
}

// Gets the pointer size.
fn get_ptr_size(bytecode: &Vec<u8>) -> usize { bytecode[0] as usize }

// Gets whether or not runtime errors have extra info.
fn get_detailed_err(bytecode: &Vec<u8>) -> bool { bytecode[1] != 0}

// Removes an integer from the stack if possible and returns it.
fn pop_int_from_stack(stack: &mut Vec<u8>) -> Option<i32>
{
    if stack.len() < 4
    {
        None
    }
    else 
    {
        let mut bytes : [u8; 4] = [0, 0, 0, 0];
        for i in 0..4
        {
            bytes[4 - i - 1] = stack.pop().expect("stack was checked");
        }
        Some(i32::from_le_bytes(bytes))
    }
}