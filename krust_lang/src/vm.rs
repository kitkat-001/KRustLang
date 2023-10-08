//! The module for the virtual machine used by the language.

use crate::{compiler, util::log};
use compiler::OpCode;
use log::{is_error, ErrorType, Log, LogType};
use std::cmp::Ordering;
use std::fmt::Display;
use std::ops::{BitAnd, BitOr, BitXor, Not};

use num_traits::FromPrimitive;

// Contains info about a runtime error that could happen.
struct RuntimeError<'a, T> {
    condition: &'a (dyn Fn(T) -> bool),
    error: ErrorType,
    index: &'a mut usize,
    bytecode: &'a Vec<u8>,
}

// A trait for types that can be stored on the stack.
trait StackType: Copy + Display + Sized {
    // The size (in bytes) of values of the type.
    fn size() -> usize;

    // The default value of the type.
    fn default() -> Self;

    // Pushes the value to the stack.
    fn push_to_stack(&self, stack: &mut Vec<u8>);

    // Pops the value from the stack.
    fn pop_from_stack(stack: &mut Vec<u8>) -> Option<Self>;

    // Reads the value from the stack at a certain index.
    fn read_from_stack(stack: &[u8], index: usize) -> Option<Self>;

    // Writes a value to the stack at a certain index. Returns whether or not this is successful.
    fn write_to_stack(&self, stack: &mut Vec<u8>, index: usize) -> bool;

    // Returns whether or not two values are equal.
    fn eq(a: Self, b: Self) -> bool;

    // Returns whether or not two values are not equal.
    fn ineq(a: Self, b: Self) -> bool;
}

macro_rules! add_eq {
    () => {
        fn eq(a: Self, b: Self) -> bool {
            a == b
        }

        fn ineq(a: Self, b: Self) -> bool {
            a != b
        }
    };
}

impl StackType for u8 {
    fn size() -> usize {
        1
    }

    fn default() -> Self {
        0u8
    }

    fn push_to_stack(&self, stack: &mut Vec<u8>) {
        stack.push(*self);
    }

    fn pop_from_stack(stack: &mut Vec<u8>) -> Option<Self> {
        stack.pop()
    }

    fn read_from_stack(stack: &[u8], index: usize) -> Option<Self> {
        stack.get(index).copied()
    }

    fn write_to_stack(&self, stack: &mut Vec<u8>, index: usize) -> bool {
        let result = index < stack.len();
        if result {
            stack[index] = *self;
        }
        result
    }

    add_eq!();
}

impl StackType for i32 {
    fn size() -> usize {
        4
    }

    fn push_to_stack(&self, stack: &mut Vec<u8>) {
        stack.append(&mut self.to_le_bytes().to_vec());
    }

    fn default() -> Self {
        0i32
    }

    fn pop_from_stack(stack: &mut Vec<u8>) -> Option<Self> {
        if stack.len() < 4 {
            None
        } else {
            let mut bytes: [u8; 4] = [0, 0, 0, 0];
            for i in 0..4 {
                bytes[4 - i - 1] = stack.pop().expect("stack was checked");
            }
            Some(Self::from_le_bytes(bytes))
        }
    }

    fn read_from_stack(stack: &[u8], index: usize) -> Option<Self> {
        if index + 4 > stack.len() {
            None
        } else {
            let mut bytes: [u8; 4] = [0, 0, 0, 0];
            bytes.copy_from_slice(&stack[index..(4 + index)]);
            Some(Self::from_le_bytes(bytes))
        }
    }

    fn write_to_stack(&self, stack: &mut Vec<u8>, index: usize) -> bool {
        let result = index + 4 <= stack.len();
        if result {
            let bytes: [u8; 4] = self.to_le_bytes();
            stack[index..(4 + index)].copy_from_slice(&bytes);
        }
        result
    }

    add_eq!();
}

impl StackType for bool {
    fn size() -> usize {
        1
    }

    fn default() -> Self {
        false
    }

    fn push_to_stack(&self, stack: &mut Vec<u8>) {
        stack.push(u8::from(*self));
    }

    fn pop_from_stack(stack: &mut Vec<u8>) -> Option<Self> {
        stack.pop().map(|value| value != 0)
    }

    fn read_from_stack(stack: &[u8], index: usize) -> Option<Self> {
        stack.get(index).copied().map(|value| value != 0)
    }

    fn write_to_stack(&self, stack: &mut Vec<u8>, index: usize) -> bool {
        let result = index < stack.len();
        if result {
            stack[index] = u8::from(*self);
        }
        result
    }

    add_eq!();
}

// A trait for numerical types.
trait NumType: StackType {
    // Negates the given value.
    fn neg(a: Self) -> Self;

    // Adds the given values.
    fn add(a: Self, b: Self) -> Self;

    // Subtracts the given values.
    fn sub(a: Self, b: Self) -> Self;

    // Multiplies the given values.
    fn mul(a: Self, b: Self) -> Self;

    // Divides the given values. Outputs zero if b is zero.
    fn div(a: Self, b: Self) -> Self;

    // Gets the remainder after division of the given values. Outputs zero if b is zero.
    fn rem(a: Self, b: Self) -> Self;

    // Compares two values with '<'.
    fn les(a: Self, b: Self) -> bool;

    // Compares two values with '<='.
    fn leq(a: Self, b: Self) -> bool;

    // Compares two values with '>'.
    fn grt(a: Self, b: Self) -> bool;

    // Compares two values with '>='.
    fn geq(a: Self, b: Self) -> bool;

    // Returns whether or not the value is 0.
    fn is_zero(a: Self) -> bool;
}

// A trait for integral types.
trait IntegralType: NumType + Not + BitAnd + BitXor + BitOr {
    // Shifts a. If b is positive, shift left. Otherwise, shift right.
    fn shift(a: Self, b: Self) -> Self;
}

macro_rules! implIntegralType {
    ($type: ty) => {
        impl NumType for $type {
            fn neg(a: Self) -> Self {
                <$type>::wrapping_neg(a)
            }

            fn add(a: Self, b: Self) -> Self {
                <$type>::wrapping_add(a, b)
            }

            fn sub(a: Self, b: Self) -> Self {
                <$type>::wrapping_sub(a, b)
            }

            fn mul(a: Self, b: Self) -> Self {
                <$type>::wrapping_mul(a, b)
            }

            fn div(a: Self, b: Self) -> Self {
                if b == 0 {
                    0
                } else {
                    <$type>::wrapping_div(a, b)
                }
            }

            fn rem(a: Self, b: Self) -> Self {
                if b == 0 {
                    0
                } else {
                    <$type>::wrapping_rem_euclid(a, b)
                }
            }

            fn les(a: Self, b: Self) -> bool {
                a < b
            }

            fn leq(a: Self, b: Self) -> bool {
                a <= b
            }

            fn grt(a: Self, b: Self) -> bool {
                a > b
            }

            fn geq(a: Self, b: Self) -> bool {
                a >= b
            }

            fn is_zero(a: Self) -> bool {
                a == 0
            }
        }

        impl IntegralType for $type {
            #[allow(unused_comparisons)]
            fn shift(a: Self, b: Self) -> Self {
                if b as isize > <$type>::BITS as isize - 1 {
                    return 0;
                }
                if (b as isize) < { -(<$type>::BITS as isize) + 1 } {
                    return if a < 0 { <$type>::wrapping_neg(1) } else { 0 }; // syntax always valid but a is never less than 0 for unsigned types.
                }

                match b.cmp(&0) {
                    Ordering::Equal => a,
                    Ordering::Greater => <$type>::wrapping_shl(a, b.try_into().unwrap_or(0)),
                    Ordering::Less => {
                        <$type>::wrapping_shr(a, <$type>::wrapping_neg(b).try_into().unwrap_or(0))
                    }
                }
            }
        }
    };
}

implIntegralType!(u8);
implIntegralType!(i32);

/// Runs the bytecode.
#[must_use]
pub fn run(bytecode: &Vec<u8>) -> (Vec<String>, Vec<Log>) {
    let mut output: Vec<String> = Vec::new();
    let mut logs: Vec<Log> = Vec::new();

    let possible_error = handle_errors(bytecode, &mut output, &mut logs);
    if let Some(error) = possible_error {
        return (error.0.clone(), error.1.clone());
    }

    let mut index: usize = 2;
    let mut stack: Vec<u8> = Vec::new();
    let mut var_list: Vec<usize> = Vec::new();
    while index < bytecode.len() {
        let curr_op: Option<OpCode> = FromPrimitive::from_u8(bytecode[index]);
        index += 1;

        if let Some(op) = curr_op {
            if match_op(
                op,
                bytecode,
                &mut stack,
                &mut index,
                &mut output,
                &mut logs,
                &mut var_list,
            ) {
                return (output, logs);
            }
        } else {
            logs.push(Log {
                log_type: LogType::Error(ErrorType::FatalError),
                line_and_col: None,
            });
            return (output, logs);
        }
    }
    (output, logs)
}

// Handle any errors immediatly present in the bytecode.
fn handle_errors<'o, 'e>(
    bytecode: &Vec<u8>,
    output: &'o mut Vec<String>,
    logs: &'e mut Vec<Log>,
) -> Option<(&'o Vec<String>, &'e Vec<Log>)> {
    if bytecode.len() < 2 {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::FatalError),
            line_and_col: None,
        });
        return Some((output, logs));
    }
    let ptr_size: usize = bytecode[0] as usize;
    if ptr_size * 8
        > usize::BITS
            .try_into()
            .expect("max value of usize must be less than the number of bits")
    {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::CompiledForDifferentTarget(ptr_size * 8)),
            line_and_col: None,
        });
        return Some((output, logs));
    }
    None
}

// Runs a function given a specific op code.
fn match_op(
    op: OpCode,
    bytecode: &Vec<u8>,
    stack: &mut Vec<u8>,
    index: &mut usize,
    output: &mut Vec<String>,
    logs: &mut Vec<Log>,
    var_list: &mut Vec<usize>,
) -> bool {
    match op {
        OpCode::PushInt => push::<i32>(bytecode, stack, index, logs),
        OpCode::PushByte => push::<u8>(bytecode, stack, index, logs),
        OpCode::PopInt => pop::<i32>(stack, logs),
        OpCode::PopByte => pop::<u8>(stack, logs),
        OpCode::PrintInt => print::<i32>(stack, output, logs),
        OpCode::PrintBool => print::<bool>(stack, output, logs),

        OpCode::AllocInt => alloc::<i32>(stack, var_list),
        OpCode::AllocBool => alloc::<bool>(stack, var_list),
        OpCode::GetInt => get::<i32>(bytecode, stack, index, logs, var_list),
        OpCode::GetBool => get::<bool>(bytecode, stack, index, logs, var_list),
        OpCode::SetInt => set::<i32>(bytecode, stack, index, logs, var_list),
        OpCode::SetBool => set::<bool>(bytecode, stack, index, logs, var_list),

        OpCode::MinusInt => minus::<i32>(stack, logs),
        OpCode::AddInt => add::<i32>(stack, logs),
        OpCode::SubtractInt => subtract::<i32>(stack, logs),
        OpCode::MultiplyInt => multiply::<i32>(stack, logs),
        OpCode::DivideInt => divide::<i32>(bytecode, stack, index, logs),
        OpCode::ModuloInt => modulo::<i32>(bytecode, stack, index, logs),

        OpCode::LessInt => less::<i32>(stack, logs),
        OpCode::LessEqualInt => less_equal::<i32>(stack, logs),
        OpCode::GreaterInt => greater::<i32>(stack, logs),
        OpCode::GreaterEqualInt => greater_equal::<i32>(stack, logs),

        OpCode::Not => not(stack, logs),

        OpCode::ComplementInt => complement::<i32>(stack, logs),
        OpCode::AndInt => and::<i32>(stack, logs),
        OpCode::AndByte => and::<u8>(stack, logs),
        OpCode::XorInt => xor::<i32>(stack, logs),
        OpCode::XorByte => xor::<u8>(stack, logs),
        OpCode::OrInt => or::<i32>(stack, logs),
        OpCode::OrByte => or::<u8>(stack, logs),

        OpCode::LeftShiftInt => left_shift::<i32>(stack, logs),
        OpCode::RightShiftInt => right_shift::<i32>(stack, logs),

        OpCode::EqualityInt => equality::<i32>(stack, logs),
        OpCode::EqualityByte => equality::<u8>(stack, logs),
        OpCode::InequalityInt => inequality::<i32>(stack, logs),
        OpCode::InequalityByte => inequality::<u8>(stack, logs),
    };
    is_error(logs)
}

// Pushes a value from the bytecode to the stack.
fn push<T>(bytecode: &Vec<u8>, stack: &mut Vec<u8>, index: &mut usize, logs: &mut Vec<Log>)
where
    T: StackType,
{
    if *index + T::size() > bytecode.len() {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::FatalError),
            line_and_col: None,
        });
    }
    for _i in 0..T::size() {
        stack.push(bytecode[*index]);
        *index += 1;
    }
}

fn pop<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: StackType,
{
    let value: Option<T> = T::pop_from_stack(stack);
    if value.is_none() {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::FatalError),
            line_and_col: None,
        });
    }
}

// Pops a value from the stack and adds it to the output.
fn print<T>(stack: &mut Vec<u8>, output: &mut Vec<String>, logs: &mut Vec<Log>)
where
    T: StackType,
{
    let value: Option<T> = T::pop_from_stack(stack);
    if let Some(value) = value {
        output.push(format!("{value}"));
    } else {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::FatalError),
            line_and_col: None,
        });
    }
}

// Allocates a variable onto the stack.
fn alloc<T>(stack: &mut Vec<u8>, var_list: &mut Vec<usize>)
where
    T: StackType,
{
    var_list.push(stack.len());
    <T>::default().push_to_stack(stack);
    <T>::default().push_to_stack(stack);
}

// Gets the value of a variable.
fn get<T>(
    bytecode: &Vec<u8>,
    stack: &mut Vec<u8>,
    index: &mut usize,
    logs: &mut Vec<Log>,
    var_list: &mut Vec<usize>,
) where
    T: StackType,
{
    let var_index: Option<usize> = get_var_index(bytecode, index);
    if let Some(var_index) = var_index {
        if var_index < var_list.len() {
            let var: Option<T> = T::read_from_stack(stack, var_list[var_index]);
            if let Some(var) = var {
                var.push_to_stack(stack);
                return;
            }
        }
    }
    logs.push(Log {
        log_type: LogType::Error(ErrorType::FatalError),
        line_and_col: None,
    });
}

// Sets the value of a variable.
fn set<T>(
    bytecode: &Vec<u8>,
    stack: &mut Vec<u8>,
    index: &mut usize,
    logs: &mut Vec<Log>,
    var_list: &mut Vec<usize>,
) where
    T: StackType,
{
    let var_index: Option<usize> = get_var_index(bytecode, index);
    if let Some(var_index) = var_index {
        if var_index < var_list.len() {
            let popped_value: Option<T> = T::pop_from_stack(stack);
            if let Some(value) = popped_value {
                if value.write_to_stack(stack, var_list[var_index]) {
                    value.push_to_stack(stack);
                    return;
                }
            }
        }
    }
    logs.push(Log {
        log_type: LogType::Error(ErrorType::FatalError),
        line_and_col: None,
    });
}

// Gets the negative of an value.
fn minus<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType,
{
    unary(stack, logs, T::neg);
}

// Adds two values.
fn add<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType,
{
    binary(stack, logs, T::add, None);
}

// Subtracts two values.
fn subtract<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType,
{
    binary(stack, logs, T::sub, None);
}

// Multiplies two values.
fn multiply<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType,
{
    binary(stack, logs, T::mul, None);
}

// Divides two values. Reports an error if the second value is zero.
fn divide<T>(bytecode: &Vec<u8>, stack: &mut Vec<u8>, index: &mut usize, logs: &mut Vec<Log>)
where
    T: NumType,
{
    binary(
        stack,
        logs,
        T::div,
        Some(RuntimeError::<(T, T)> {
            condition: &(|(_a, b)| <T>::is_zero(b)),
            error: ErrorType::DivideByZero,
            index,
            bytecode,
        }),
    );
}

// Gets the modulo of two values. Reports an error if the second value is zero.
fn modulo<T>(bytecode: &Vec<u8>, stack: &mut Vec<u8>, index: &mut usize, logs: &mut Vec<Log>)
where
    T: NumType,
{
    binary(
        stack,
        logs,
        T::rem,
        Some(RuntimeError::<(T, T)> {
            condition: &(|(_a, b)| <T>::is_zero(b)),
            error: ErrorType::DivideByZero,
            index,
            bytecode,
        }),
    );
}

// Compares two values with '<'.
fn less<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType,
{
    binary(stack, logs, T::les, None);
}

// Compares two values with '<='.
fn less_equal<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType,
{
    binary(stack, logs, T::leq, None);
}

// Compares two values with '>'.
fn greater<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType,
{
    binary(stack, logs, T::grt, None);
}

// Compares two values with '>='.
fn greater_equal<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType,
{
    binary(stack, logs, T::geq, None);
}

// Negates a bool.
fn not(stack: &mut Vec<u8>, logs: &mut Vec<Log>) {
    unary(stack, logs, |a: u8| a ^ 1);
}

// Finds the complement of an value.
fn complement<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType + Not,
    <T as Not>::Output: StackType,
{
    unary(stack, logs, T::not);
}

// Bitwise ands two values.
fn and<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType + BitAnd,
    <T as BitAnd>::Output: StackType,
{
    binary(stack, logs, T::bitand, None);
}

// Bitwise xors two values.
fn xor<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType + BitXor,
    <T as BitXor>::Output: StackType,
{
    binary(stack, logs, T::bitxor, None);
}

// Bitwise ors two values.
fn or<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: NumType + BitOr,
    <T as BitOr>::Output: StackType,
{
    binary(stack, logs, T::bitor, None);
}
// Left shifts a value by another value.
fn left_shift<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: IntegralType,
{
    binary(stack, logs, T::shift, None);
}

// Right shifts a value by another value.
fn right_shift<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: IntegralType,
{
    binary(stack, logs, |a: T, b: T| T::shift(a, <T>::neg(b)), None);
}

// Checks if two values are equal.
fn equality<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: StackType,
{
    binary(stack, logs, <T>::eq, None);
}

// Checks if two values are not equal.
fn inequality<T>(stack: &mut Vec<u8>, logs: &mut Vec<Log>)
where
    T: StackType,
{
    binary(stack, logs, <T>::ineq, None);
}

// Performs a unary operation.
fn unary<F, T, TOut>(stack: &mut Vec<u8>, logs: &mut Vec<Log>, func: F)
where
    F: Fn(T) -> TOut,
    T: StackType,
    TOut: StackType,
{
    let value: Option<T> = T::pop_from_stack(stack);
    if let Some(value) = value {
        let value: TOut = func(value);
        value.push_to_stack(stack);
    } else {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::FatalError),
            line_and_col: None,
        });
    }
}

// Performs a binary operation.
fn binary<F, T1, T2, TOut>(
    stack: &mut Vec<u8>,
    logs: &mut Vec<Log>,
    func: F,
    error: Option<RuntimeError<(T1, T2)>>,
) where
    F: Fn(T1, T2) -> TOut,
    T1: StackType,
    T2: StackType,
    TOut: StackType,
{
    let detailed_err: bool = if let Some(error) = &error {
        get_detailed_err(error.bytecode)
    } else {
        false
    };
    if detailed_err && errors_stored_incorrectly(error.as_ref().expect("detailed_err is true")) {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::FatalError),
            line_and_col: None,
        });
        return;
    }

    let b: Option<T2> = T2::pop_from_stack(stack);
    let a: Option<T1> = T1::pop_from_stack(stack);
    let mut fail: bool = true;
    if let Some(a) = a {
        if let Some(b) = b {
            fail = false;
            if let Some(mut error) = error {
                handle_error(&mut error, (a, b), detailed_err, logs);
            }
            let c: TOut = func(a, b);
            c.push_to_stack(stack);
        }
    }
    if fail {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::FatalError),
            line_and_col: None,
        });
    }
}

// Checks to make sure that line info is properly stored in operations that may have an error.
fn errors_stored_incorrectly<T>(error: &RuntimeError<T>) -> bool {
    let index: usize = *error.index;
    let bytecode: &Vec<u8> = error.bytecode;
    index + 2 * get_ptr_size(bytecode) > bytecode.len()
}

// Handles runtime errors.
fn handle_error<T>(error: &mut RuntimeError<T>, value: T, detailed_err: bool, logs: &mut Vec<Log>) {
    let condition: &dyn Fn(T) -> bool = error.condition;
    let index: &mut usize = error.index;
    let bytecode: &Vec<u8> = error.bytecode;
    let ptr_size: usize = get_ptr_size(bytecode);
    if condition(value) {
        if detailed_err {
            let mut bytes: [u8; (usize::BITS / 8) as usize] = [0; (usize::BITS / 8) as usize];
            for byte in bytes.iter_mut().take(ptr_size) {
                *byte = bytecode[*index];
                *index += 1;
            }
            let line: usize = usize::from_le_bytes(bytes);
            bytes = [0; (usize::BITS / 8) as usize];
            for byte in bytes.iter_mut().take(ptr_size) {
                *byte = bytecode[*index];
                *index += 1;
            }
            let col: usize = usize::from_le_bytes(bytes);
            logs.push(Log {
                log_type: LogType::Error(error.error.clone()),
                line_and_col: Some((line, col)),
            });
        } else {
            logs.push(Log {
                log_type: LogType::Error(error.error.clone()),
                line_and_col: None,
            });
        }
    } else if detailed_err {
        *index += 2 * get_ptr_size(bytecode);
    }
}

// Gets the variable index from the bytecode if availible.
fn get_var_index(bytecode: &Vec<u8>, index: &mut usize) -> Option<usize> {
    // TODO: Make sure BYTES_PER_VAR is at most ptr_size.
    if *index + compiler::BYTES_PER_VAR > bytecode.len() {
        None
    } else {
        let mut bytes: [u8; (usize::BITS / 8) as usize] = [0; (usize::BITS / 8) as usize];
        for byte in bytes.iter_mut().take(compiler::BYTES_PER_VAR) {
            *byte = bytecode[*index];
            *index += 1;
        }
        Some(usize::from_le_bytes(bytes))
    }
}

// Gets the pointer size.
fn get_ptr_size(bytecode: &[u8]) -> usize {
    bytecode[0] as usize
}

// Gets whether or not runtime errors have extra info.
fn get_detailed_err(bytecode: &[u8]) -> bool {
    bytecode[1] != 0
}

/// The module for test functions
pub mod test_func {
    use super::IntegralType;

    #[must_use]
    pub fn shift_int(a: i32, b: i32) -> i32 {
        i32::shift(a, b)
    }
}
