//! The module for the virtual machine used by the language.

use crate::{log, math, compiler};
use log::{Log, LogType, ErrorType, is_error};
use math::shift_int;
use compiler::OpCode;

use num_traits::FromPrimitive;

/// Runs the bytecode.
pub fn run(bytecode: &Vec<u8>) -> (Vec<String>, Vec<Log>)
{
    let mut output: Vec<String> = Vec::new();
    let mut logs: Vec<Log> = Vec::new();

    let possible_error = handle_errors(bytecode, &mut output, &mut logs);
    let ptr_size: Option<usize> = possible_error.1;
    let possible_error: Option<(&Vec<String>, &Vec<Log>)> = possible_error.0;
    if possible_error.is_some()
    {
        let error: (&Vec<String>, &Vec<Log>) = possible_error.expect("if statement");
        return (error.0.clone(), error.1.clone());
    }
    let ptr_size: usize = ptr_size.expect("no error yet");

    let mut index: usize = 1;
    let mut stack: Vec<u8> = Vec::new();
    while index < bytecode.len()
    {
        let curr_op: Option<OpCode> = FromPrimitive::from_u8(bytecode[index]);
        index += 1;

        match curr_op
        {
            Some(op) => 
            {
                if match_op(op, bytecode, &mut stack, &mut index, &mut output, &mut logs, ptr_size)
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
    -> (Option<(&'o Vec<String>, &'e Vec<Log>)>, Option<usize>)
{
    if bytecode.len() < 1
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
        return (Some((output, logs)), None);
    }
    let ptr_size: usize = bytecode[0] as usize;
    if ptr_size * 8 > usize::BITS.try_into().expect("max value of usize must be less than the number of bits")
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::CompiledForDifferentTarget(ptr_size * 8)), line_and_col: None});
        return (Some((output, logs)), Some(ptr_size));
    }
    (None, Some(ptr_size))
}

// Runs a function given a specific op code.
fn match_op(
    op: OpCode, 
    bytecode: &Vec<u8>, 
    stack: &mut Vec<u8>,
    index: &mut usize,
    output: &mut Vec<String>,
    logs: &mut Vec<Log>,
    ptr_size: usize
) -> bool
{
    match op
    {
        OpCode::PushInt => push_int(bytecode, stack, index, logs),
        OpCode::PopInt => pop_int(stack, output, logs),
        OpCode::MinusInt => minus_int(stack, logs),
        OpCode::AddInt => add_int(stack, logs),
        OpCode::SubtractInt => subtract_int(stack, logs),
        OpCode::MultiplyInt => multiply_int(stack, logs),
        OpCode::DivideInt => divide_int(bytecode, stack, index, logs, ptr_size),
        OpCode::ModuloInt => modulo_int(bytecode, stack, index, logs, ptr_size),
        OpCode::ComplementInt => complement_int(stack, logs),
        OpCode::LeftShiftInt => left_shift_int(stack, logs),
        OpCode::RightShiftInt => right_shift_int(stack, logs),
        OpCode::AndInt => and_int(stack, logs),
        OpCode::XorInt => xor_int(stack, logs),
        OpCode::OrInt => or_int(stack, logs),
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

// Pops an int from the stack and adds it to the output.
fn pop_int(stack: &mut Vec<u8>, output: &mut Vec<String>, logs: &mut Vec<Log>,)
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

// Gets the negative of an int.
fn minus_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    let value: Option<i32> = pop_int_from_stack(stack);
    if let Some(value) = value
    {
        let value: i32 = i32::wrapping_neg(value);
        stack.append(&mut value.to_le_bytes().to_vec());
    }
    else 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Adds two ints.
fn add_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    let b: Option<i32> = pop_int_from_stack(stack);
    let a: Option<i32> = pop_int_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            let c: i32 = i32::wrapping_add(a, b);
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Subtracts two ints.
fn subtract_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    let b: Option<i32> = pop_int_from_stack(stack);
    let a: Option<i32> = pop_int_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            let c: i32 = i32::wrapping_sub(a, b);
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Multiplies two ints.
fn multiply_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    let b: Option<i32> = pop_int_from_stack(stack);
    let a: Option<i32> = pop_int_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            let c: i32 = i32::wrapping_mul(a, b);
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Divides two ints. Reports an error if the second int is zero.
fn divide_int(bytecode: &Vec<u8>,  stack: &mut Vec<u8>, index: &mut usize, logs: &mut Vec<Log>, ptr_size: usize)
{
    if *index + 2 * ptr_size > bytecode.len()
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
    
    let b: Option<i32> = pop_int_from_stack(stack);
    let a: Option<i32> = pop_int_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            if b == 0
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
                logs.push(Log{log_type: LogType::Error(ErrorType::DivideByZero), line_and_col: Some((line, col))});
                return;
            }
            let c: i32 = i32::wrapping_div(a, b);
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
    *index += 2 * ptr_size as usize;
}

// Gets the modulo of two ints. Reports an error if the second int is zero.
fn modulo_int(bytecode: &Vec<u8>,  stack: &mut Vec<u8>, index: &mut usize, logs: &mut Vec<Log>, ptr_size: usize)
{
    if *index + 2 * ptr_size > bytecode.len()
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
    
    let b: Option<i32> = pop_int_from_stack(stack);
    let a: Option<i32> = pop_int_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            if b == 0
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
                logs.push(Log{log_type: LogType::Error(ErrorType::DivideByZero), line_and_col: Some((line, col))});
                return;
            }
            let c: i32 = i32::wrapping_rem_euclid(a, b);
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
    *index += 2 * ptr_size as usize;
}

// Finds the complement of an int.
fn complement_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    let value: Option<i32> = pop_int_from_stack(stack);
    if let Some(value) = value
    {
        let value: i32 = !value;
        stack.append(&mut value.to_le_bytes().to_vec());
    }
    else 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}


// Left shifts an int by an int
fn left_shift_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    let b: Option<i32> = pop_int_from_stack(stack);
    let a: Option<i32> = pop_int_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            let c: i32 = shift_int(a, b);
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Left shifts an int by an int
fn right_shift_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    let b: Option<i32> = pop_int_from_stack(stack);
    let a: Option<i32> = pop_int_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            let c: i32 = math::shift_int(a, -b);
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Bitwise ands two ints.
fn and_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    let b: Option<i32> = pop_int_from_stack(stack);
    let a: Option<i32> = pop_int_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            let c: i32 = a & b;
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Bitwise xors two ints.
fn xor_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    let b: Option<i32> = pop_int_from_stack(stack);
    let a: Option<i32> = pop_int_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            let c: i32 = a ^ b;
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Bitwise ors two ints.
fn or_int(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
{
    let b: Option<i32> = pop_int_from_stack(stack);
    let a: Option<i32> = pop_int_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a
    {
        if let Some(b) = b
        {   
            fail = false;
            let c: i32 = a | b;
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

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