//! The module for the virtual machine used by the language.

use crate::compiler;
use crate::math;
use compiler::OpCode;
use math::shift_int;

use num_traits::FromPrimitive;

/// Runs the bytecode.
pub fn run(bytecode: &Vec<u8>) -> (Vec<String>, Option<String>)
{
    let mut output: Vec<String> = Vec::new();
    let mut err: Option<String> =None;

    let possible_error = handle_errors(bytecode, &mut output, &mut err);
    let ptr_size: Option<usize> = possible_error.1;
    let possible_error: Option<(&Vec<String>, &Option<String>)> = possible_error.0;
    if possible_error.is_some()
    {
        let error: (&Vec<String>, &Option<String>) = possible_error.expect("if statement");
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
                if match_op(op, bytecode, &mut stack, &mut index, &mut output, &mut err, ptr_size)
                {
                    return (output, err);
                }
            }
            None => 
            { 
                err = Some("fatal error; program terminated".to_string());
                return (output, err);
            }
        }
    }
    (output, err)
}

// Handle any errors immediatly present in the bytecode.
fn handle_errors<'o, 'e>(bytecode: &Vec<u8>, output: &'o mut Vec<String>, err: &'e mut Option<String>) 
    -> (Option<(&'o Vec<String>, &'e Option<String>)>, Option<usize>)
{
    if bytecode.len() < 1
    {
        *err = Some("fatal error; program terminated".to_string());
        return (Some((output, err)), None);
    }
    let ptr_size: usize = bytecode[0] as usize;
    if ptr_size * 8 > usize::BITS.try_into().expect("max value of usize must be less than the number of bits")
    {
        *err = Some(format!("error:  this program was compiled for a {}-bit machine, while this is only a {}-bit machine.", 
            ptr_size * 8, usize::BITS));
        return (Some((output, err)), Some(ptr_size));
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
    err: &mut Option<String>,
    ptr_size: usize
) -> bool
{
    match op
    {
        OpCode::PushInt => push_int(bytecode, stack, index, err),
        OpCode::PopInt => pop_int(stack, output, err),
        OpCode::MinusInt => minus_int(stack, err),
        OpCode::AddInt => add_int(stack, err),
        OpCode::SubtractInt => subtract_int(stack, err),
        OpCode::MultiplyInt => multiply_int(stack, err),
        OpCode::DivideInt => divide_int(bytecode, stack, index, err, ptr_size),
        OpCode::ModuloInt => modulo_int(bytecode, stack, index, err, ptr_size),
        OpCode::ComplementInt => complement_int(stack, err),
        OpCode::LeftShiftInt => left_shift_int(stack, err),
        OpCode::RightShiftInt => right_shift_int(stack, err),
        OpCode::AndInt => and_int(stack, err),
        OpCode::XorInt => xor_int(stack, err),
        OpCode::OrInt => or_int(stack, err),
    };
    err.is_some()
}

// Pushes an int from the bytecode to the stack.
fn push_int(bytecode: &Vec<u8>, stack: &mut Vec<u8>, index: &mut usize, err: &mut Option<String>)
{
    if *index + 4 > bytecode.len()
    {
        *err = Some("fatal error; program terminated".to_string());
    }
    for _i in 0..4
    {
        stack.push(bytecode[*index]);
        *index += 1;
    }
}

// Pops an int from the stack and adds it to the output.
fn pop_int(stack: &mut Vec<u8>, output: &mut Vec<String>, err: &mut Option<String>,)
{
    let value: Option<i32> = pop_int_from_stack(stack);
    if let Some(value) = value
    {
        output.push(format!("{}", value));
    }
    else
    {
        *err = Some("fatal error; program terminated".to_string());
    }
}

// Gets the negative of an int.
fn minus_int(stack: &mut Vec<u8>, err: &mut Option<String>)
{
    let value: Option<i32> = pop_int_from_stack(stack);
    if let Some(value) = value
    {
        let value: i32 = i32::wrapping_neg(value);
        stack.append(&mut value.to_le_bytes().to_vec());
    }
    else 
    {
        *err = Some("fatal error; program terminated".to_string());
    }
}

// Adds two ints.
fn add_int(stack: &mut Vec<u8>, err: &mut Option<String>)
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
        *err = Some("fatal error; program terminated".to_string());
    }
}

// Subtracts two ints.
fn subtract_int(stack: &mut Vec<u8>, err: &mut Option<String>)
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
        *err = Some("fatal error; program terminated".to_string());
    }
}

// Multiplies two ints.
fn multiply_int(stack: &mut Vec<u8>, err: &mut Option<String>)
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
        *err = Some("fatal error; program terminated".to_string());
    }
}

// Divides two ints. Reports an error if the second int is zero.
fn divide_int(bytecode: &Vec<u8>,  stack: &mut Vec<u8>, index: &mut usize, err: &mut Option<String>, ptr_size: usize)
{
    if *index + 2 * ptr_size > bytecode.len()
    {
        *err = Some("fatal error; program terminated".to_string());
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
                *err =  Some(format!("error (line {line}:{col}): division by 0"));
                return;
            }
            let c: i32 = i32::wrapping_div(a, b);
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        *err = Some("fatal error; program terminated".to_string());
    }
    *index += 2 * ptr_size as usize;
}

// Gets the modulo of two ints. Reports an error if the second int is zero.
fn modulo_int(bytecode: &Vec<u8>,  stack: &mut Vec<u8>, index: &mut usize, err: &mut Option<String>, ptr_size: usize)
{
    if *index + 2 * ptr_size > bytecode.len()
    {
        *err = Some("fatal error; program terminated".to_string());
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
                *err =  Some(format!("error (line {line}:{col}): modulo by 0"));
                return;
            }
            let c: i32 = i32::wrapping_rem_euclid(a, b);
            stack.append(&mut c.to_le_bytes().to_vec());
        }
    }
    if fail 
    {
        *err = Some("fatal error; program terminated".to_string());
    }
    *index += 2 * ptr_size as usize;
}

// Finds the complement of an int.
fn complement_int(stack: &mut Vec<u8>, err: &mut Option<String>)
{
    let value: Option<i32> = pop_int_from_stack(stack);
    if let Some(value) = value
    {
        let value: i32 = !value;
        stack.append(&mut value.to_le_bytes().to_vec());
    }
    else 
    {
        *err = Some("fatal error; program terminated".to_string());
    }
}


// Left shifts an int by an int
fn left_shift_int(stack: &mut Vec<u8>, err: &mut Option<String>)
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
        *err = Some("fatal error; program terminated".to_string());
    }
}

// Left shifts an int by an int
fn right_shift_int(stack: &mut Vec<u8>, err: &mut Option<String>)
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
        *err = Some("fatal error; program terminated".to_string());
    }
}

// Bitwise ands two ints.
fn and_int(stack: &mut Vec<u8>, err: &mut Option<String>)
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
        *err = Some("fatal error; program terminated".to_string());
    }
}

// Bitwise xors two ints.
fn xor_int(stack: &mut Vec<u8>, err: &mut Option<String>)
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
        *err = Some("fatal error; program terminated".to_string());
    }
}

// Bitwise ors two ints.
fn or_int(stack: &mut Vec<u8>, err: &mut Option<String>)
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
        *err = Some("fatal error; program terminated".to_string());
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