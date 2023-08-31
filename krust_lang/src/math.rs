//! The module for any math method needed in more than one module.

/// Shifts an integer by the value of another integer. 
/// If b is positive, shift left. Otherwise, shift right.
pub fn shift_int(a: i32, b: i32) -> i32
{
    if b > 31 {return 0;}
    if b < -31 {return if a < 0 {-1} else {0};}
    let b_abs: u32 = i32::wrapping_abs(b) as u32;
    if b == 0 {a}
    else if b > 0 {i32::wrapping_shl(a, b_abs)}
    else {i32::wrapping_shr(a, b_abs)}
}