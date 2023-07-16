//! The module for the virtual machine used by the language.

use crate::compiler;
use compiler::OpCodes;

use num_traits::FromPrimitive;

/// Runs the bytecode.
pub fn run(bytecode: Vec<u8>)
{
    if bytecode.len() < 1
    {
        eprintln!("fatal error; program terminated");
        return;
    }

    let ptr_size: usize = bytecode[0] as usize;
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
                        eprintln!("fatal error; program terminated");
                        return;
                    }
                    for _i in 0..4
                    {
                        stack.push(bytecode[index]);
                        index += 1;
                    }
                }
                OpCodes::PopInt =>
                {
                    let value: Option<i32> = pop_int(&mut stack);
                    if let Some(value) = value
                    {
                        println!("{}", value);
                    }
                    else 
                    {
                        eprintln!("fatal error; program terminated");
                    }
                }
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
                        eprintln!("fatal error; program terminated");
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
                        eprintln!("fatal error; program terminated");
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
                        eprintln!("fatal error; program terminated");
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
                        eprintln!("fatal error; program terminated");
                    }
                },
                OpCodes::DivideInt =>
                {
                    if index + 2 * ptr_size > bytecode.len()
                    {
                        eprintln!("fatal error; program terminated");
                        return;
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
                                eprintln!("error (line {line}:{col}): division by 0");
                                return;
                            }
                            let c: i32 = i32::wrapping_div(a, b);
                            stack.append(&mut c.to_le_bytes().to_vec());
                        }
                    }
                    if fail 
                    {
                        eprintln!("fatal error; program terminated");
                    }
                    index += 2 * ptr_size as usize;
                }
            }},
            None => { return; }
        }
    }
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