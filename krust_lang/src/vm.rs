//! The module for the virtual machine used by the language.

use crate::compiler;
use compiler::OpCodes;

use num_traits::FromPrimitive;

/// Runs the bytecode.
pub fn run(bytecode: &Vec<u8>) -> (Vec<String>, Option<String>)
{
    let mut output: Vec<String> = Vec::new();
    let mut err: Option<String> =None;

    if bytecode.len() < 1
    {
        err = Some("fatal error; program terminated".to_string());
        return (output, err);
    }
    let ptr_size: usize = bytecode[0] as usize;
    if ptr_size * 8 > usize::BITS.try_into().expect("max value of usize must be less than the number of bits")
    {
        err = Some(format!("error:  this program was compiled for a {}-bit machine, while this is only a {}-bit machine.", 
            ptr_size * 8, usize::BITS));
        return (output, err);
    }

    let mut index: usize = 1;
    let mut stack: Vec<u8> = Vec::new();
    while index < bytecode.len()
    {
        let curr_op: Option<OpCodes> = FromPrimitive::from_u8(bytecode[index]);
        index += 1;

        match curr_op
        {
            Some(op) => {match op
            {
                OpCodes::PushInt =>
                {
                    if index + 4 > bytecode.len()
                    {
                        err = Some("fatal error; program terminated".to_string());
                        return (output, err);
                    }
                    for _i in 0..4
                    {
                        stack.push(bytecode[index]);
                        index += 1;
                    }
                },
                OpCodes::PopInt =>
                {
                    let value: Option<i32> = pop_int(&mut stack);
                    if let Some(value) = value
                    {
                        output.push(format!("{}", value));
                    }
                    else
                    {
                        err = Some("fatal error; program terminated".to_string());
                        return (output, err);
                    }
                },
                OpCodes::MinusInt =>
                {
                    let value: Option<i32> = pop_int(&mut stack);
                    if let Some(value) = value
                    {
                        let value: i32 = i32::wrapping_neg(value);
                        stack.append(&mut value.to_le_bytes().to_vec());
                    }
                    else 
                    {
                        err = Some("fatal error; program terminated".to_string());
                        return (output, err);
                    }
                },
                OpCodes::AddInt =>
                {
                    let b: Option<i32> = pop_int(&mut stack);
                    let a: Option<i32> = pop_int(&mut stack);
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
                        err = Some("fatal error; program terminated".to_string());
                        return (output, err);
                    }
                },
                OpCodes::SubtractInt =>
                {
                    let b: Option<i32> = pop_int(&mut stack);
                    let a: Option<i32> = pop_int(&mut stack);
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
                        err = Some("fatal error; program terminated".to_string());
                        return (output, err);
                    }
                },
                OpCodes::MultiplyInt =>
                {
                    let b: Option<i32> = pop_int(&mut stack);
                    let a: Option<i32> = pop_int(&mut stack);
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
                        err = Some("fatal error; program terminated".to_string());
                        return (output, err);
                    }
                },
                OpCodes::DivideInt =>
                {
                    if index + 2 * ptr_size > bytecode.len()
                    {
                        err = Some("fatal error; program terminated".to_string());
                        return (output, err);
                    }
                    
                    let b: Option<i32> = pop_int(&mut stack);
                    let a: Option<i32> = pop_int(&mut stack);
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
                                    bytes[i] = bytecode[index];
                                    index += 1;
                                }
                                let line: usize = usize::from_le_bytes(bytes);
                                bytes = [0; (usize::BITS / 8) as usize];
                                for i in 0..ptr_size
                                {
                                    bytes[i] = bytecode[index];
                                    index += 1;
                                }
                                let col: usize = usize::from_le_bytes(bytes);
                                err=  Some(format!("error (line {line}:{col}): division by 0"));
                                return (output, err);
                            }
                            let c: i32 = i32::wrapping_div(a, b);
                            stack.append(&mut c.to_le_bytes().to_vec());
                        }
                    }
                    if fail 
                    {
                        err = Some("fatal error; program terminated".to_string());
                        return (output, err);
                    }
                    index += 2 * ptr_size as usize;
                }
            }},
            None => 
            { 
                err = Some("fatal error; program terminated".to_string());
                return (output, err);
            }
        }
    }
    (output, err)
}

fn pop_int(stack: &mut Vec<u8>) -> Option<i32>
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