//! The module for any math method needed in more than one module.

use std::cmp::Ordering;

/// Shifts an integer by the value of another integer.
/// If b is positive, shift left. Otherwise, shift right.
/// Note: the absolute value of b is greater than the number of bits in a, then all bits in a will be replaced. This is different in other programming langauges, were the leftmost bits of b are ignored.
#[must_use]
#[allow(clippy::cast_sign_loss)] // The abs function guarentees that the i32 being converted is positive.
pub fn shift_int(a: i32, b: i32) -> i32 {
    if b > 31 {
        return 0;
    }
    if b < -31 {
        return if a < 0 { -1 } else { 0 };
    }
    let b_abs: u32 = i32::wrapping_abs(b) as u32;

    match b.cmp(&0) {
        Ordering::Equal => a,
        Ordering::Greater => i32::wrapping_shl(a, b_abs),
        Ordering::Less => i32::wrapping_shr(a, b_abs),
    }
}
