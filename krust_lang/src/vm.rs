//! The module for the virtual machine used by the language.

use crate::{log, math, compiler};
use log::{Log, LogType, ErrorType, is_error};
use math::shift_int;
use compiler::OpCode;
use compiler::const_pool::ConstantPool;

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
    let mut const_pool: ConstantPool = ConstantPool { pool: Vec::new(), ptr_size: get_ptr_size(bytecode) };
    while index < bytecode.len()
    {
        let curr_op: Option<OpCode> = FromPrimitive::from_u8(bytecode[index]);
        index += 1;

        match curr_op
        {
            Some(op) => 
            {
                if match_op(op, bytecode, &mut stack, &mut const_pool, &mut index, &mut output, &mut logs)
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
    const_pool: &mut ConstantPool,
    index: &mut usize,
    output: &mut Vec<String>,
    logs: &mut Vec<Log>,
) -> bool
{
    match op
    {
        OpCode::Const => fill_const_pool(bytecode, const_pool, index),
        OpCode::Push => push(bytecode, stack, index, logs),
        OpCode::PopInt => pop_int(stack, const_pool, output, logs),
        OpCode::MinusInt => minus_int(stack, const_pool, logs),
        OpCode::AddInt => add_int(stack, const_pool, logs),
        OpCode::SubtractInt => subtract_int(stack, const_pool, logs),
        OpCode::MultiplyInt => multiply_int(stack, const_pool, logs),
        OpCode::DivideInt => divide_int(bytecode, stack, const_pool, index, logs),
        OpCode::ModuloInt => modulo_int(bytecode, stack, const_pool, index, logs),
        OpCode::ComplementInt => complement_int(stack, const_pool, logs),
        OpCode::LeftShiftInt => left_shift_int(stack, const_pool, logs),
        OpCode::RightShiftInt => right_shift_int(stack, const_pool, logs),
        OpCode::AndInt => and_int(stack, const_pool, logs),
        OpCode::XorInt => xor_int(stack, const_pool, logs),
        OpCode::OrInt => or_int(stack, const_pool, logs),
    };
    is_error(logs)
}

// Adds all values from the bytecode into the constant pool.
fn fill_const_pool(bytecode: &Vec<u8>, const_pool: &mut ConstantPool, index: &mut usize)
{
    let mut length_as_bytes: Vec<u8> = bytecode[*index..*index+const_pool.ptr_size as usize].to_vec();
    while length_as_bytes.len() < usize::BITS as usize / 8 as usize
    {
        length_as_bytes.push(0);
    }
    let length = usize::from_le_bytes(length_as_bytes.try_into().expect("while statement made this the right size"));
    *index += const_pool.ptr_size as usize;

    for _i in 0..length
    {
        const_pool.pool.push(bytecode[*index]);
        *index += 1;
    }
}

// Pushes an address from the bytecode to the stack.
fn push(bytecode: &Vec<u8>, stack: &mut Vec<u8>, index: &mut usize, logs: &mut Vec<Log>)
{
    if *index + get_ptr_size(bytecode) as usize > bytecode.len()
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
        return;
    }

    for _i in 0..get_ptr_size(bytecode)
    {
        stack.push(bytecode[*index]);
        *index += 1;
    }
}

// Pops an int from the stack and adds it to the output.
fn pop_int(stack: &mut Vec<u8>, const_pool: &ConstantPool, output: &mut Vec<String>, logs: &mut Vec<Log>,)
{
    let value: Option<i32> = pop_int_from_stack(stack, const_pool);
    if let Some(value) = value
    {
        output.push(format!("{value}"));
    }
    else
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Gets the negative of an int.
fn minus_int(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>)
{
    unary_int(stack, const_pool, logs, |a: i32| i32::wrapping_neg(a));
}

// Adds two ints.
fn add_int(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>)
{
    binary_int(stack, const_pool, logs, |a, b| i32::wrapping_add(a, b), None);
}

// Subtracts two ints.
fn subtract_int(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>)
{
    binary_int(stack, const_pool, logs, |a, b| i32::wrapping_sub(a, b), None);
}

// Multiplies two ints.
fn multiply_int(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>) 
{
    binary_int(stack, const_pool, logs, |a, b| i32::wrapping_mul(a, b), None);
}

// Divides two ints. Reports an error if the second int is zero.
fn divide_int(bytecode: &Vec<u8>,  stack: &mut Vec<u8>, const_pool: &mut ConstantPool, index: &mut usize, logs: &mut Vec<Log>)
{
    binary_int(stack, const_pool, logs,  |a, b| if b == 0 {0} else {i32::wrapping_div(a, b)}, 
        Some(RuntimeError::<(i32, i32)>{
            condition: &(|(_a, b)| b == 0),
            error: ErrorType::DivideByZero,
            index, bytecode
    }))
}

// Gets the modulo of two ints. Reports an error if the second int is zero.
fn modulo_int(bytecode: &Vec<u8>,  stack: &mut Vec<u8>, const_pool: &mut ConstantPool, index: &mut usize, logs: &mut Vec<Log>)
{
    binary_int(stack, const_pool, logs,  |a, b| if b == 0 {0} else {i32::wrapping_rem_euclid(a, b)}, 
        Some(RuntimeError::<(i32, i32)>{
            condition: &(|(_a, b)| b == 0),
            error: ErrorType::DivideByZero,
            index, bytecode
    }))
}

// Finds the complement of an int.
fn complement_int(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>)
{
    unary_int(stack, const_pool, logs, |a: i32| !a);
}


// Left shifts an int by an int
fn left_shift_int(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>)
{
    binary_int(stack, const_pool, logs, |a, b| shift_int(a, b), None);
}

// Left shifts an int by an int
fn right_shift_int(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>)
{
    binary_int(stack, const_pool, logs, |a, b| shift_int(a, -b), None);
}

// Bitwise ands two ints.
fn and_int(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>)
{
    binary_int(stack, const_pool, logs, |a, b| a & b, None);
}

// Bitwise xors two ints.
fn xor_int(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>)
{
    binary_int(stack, const_pool, logs, |a, b| a ^ b, None);
}

// Bitwise ors two ints.
fn or_int(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>)
{
    binary_int(stack, const_pool, logs, |a, b| a | b, None);
}

// Performs a unary operation on an int.
fn unary_int<F>(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>, func: F)
    where F: Fn(i32) -> i32
{
    let value: Option<i32> = pop_int_from_stack(stack, const_pool);
    if let Some(value) = value
    {
        let value: i32 = func(value);
        let result: Result<(), ()> = push_int_to_stack(stack, const_pool, value);
        if let Err(_) = result { logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});}
    }
    else 
    {
        logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});
    }
}

// Performs a binary operation on a pair of ints.
fn binary_int<F>(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, logs: &mut Vec<Log>, func: F, error: Option<RuntimeError<(i32, i32)>>)
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

    let b: Option<i32> = pop_int_from_stack(stack, const_pool);
    let a: Option<i32> = pop_int_from_stack(stack, const_pool);
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
            let result: Result<(), ()> = push_int_to_stack(stack, const_pool, c);
            if let Err(_) = result { logs.push(Log{log_type: LogType::Error(ErrorType::FatalError), line_and_col: None});}
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
    index + 2 * get_ptr_size(bytecode) as usize > bytecode.len()
}

// Handles runtime errors.
fn handle_error<T>(error: &mut RuntimeError<T>, value: T, detailed_err: bool, logs: &mut Vec<Log>)
{
    let condition: &dyn Fn(T) -> bool = error.condition;
    let index: &mut usize = error.index;
    let bytecode: &Vec<u8> = error.bytecode;
    let ptr_size: u8 = get_ptr_size(bytecode);
    if condition(value)
    {
        if detailed_err
        {
            let mut bytes : [u8; (usize::BITS / 8) as usize] = [0; (usize::BITS / 8) as usize];
            for i in 0..ptr_size as usize
            {
                bytes[i] = bytecode[*index];
                *index += 1;
            }
            let line: usize = usize::from_le_bytes(bytes);
            bytes = [0; (usize::BITS / 8) as usize];
            for i in 0..ptr_size as usize
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
        *index += 2 * get_ptr_size(bytecode) as usize;
    }
}

// Gets the pointer size.
fn get_ptr_size(bytecode: &Vec<u8>) -> u8 { bytecode[0] }

// Gets whether or not runtime errors have extra info.
fn get_detailed_err(bytecode: &Vec<u8>) -> bool { bytecode[1] != 0}

// Adds an int to the stack.
fn push_int_to_stack(stack: &mut Vec<u8>, const_pool: &mut ConstantPool, value: i32) -> Result<(), ()>
{
    stack.append(&mut const_pool.insert_int(value)?.to_le_bytes()[0..const_pool.ptr_size as usize].try_into().expect("should be able to become vec"));
    Ok(())
}

// Removes an integer from the stack if possible and returns it.
fn pop_int_from_stack(stack: &mut Vec<u8>, const_pool: &ConstantPool) -> Option<i32>
{
    if stack.len() < const_pool.ptr_size as usize
    {
        None
    }
    else 
    {
        let mut bytes : Vec<u8> = Vec::new();
        for _i in 0..const_pool.ptr_size as usize
        {
            bytes.push(stack.pop().expect("stack was checked"));
        }
        bytes.reverse();
        while bytes.len() < (usize::BITS / 8) as usize
        {
            bytes.push(0);
        }

        const_pool.get_int(usize::from_le_bytes(bytes.try_into().expect("while loop made it right size")))
    }
}