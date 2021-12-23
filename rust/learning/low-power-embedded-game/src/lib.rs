//! <https://exercism.org/tracks/rust/exercises/low-power-embedded-game>

/// Calculate the quotient and remainder of a division.
///
/// # Panics
///
/// If division by zero or overflow occurs.
pub fn divmod(dividend: i16, divisor: i16) -> (i16, i16) {
    let div = dividend.checked_div_euclid(divisor).unwrap();
    let rem = dividend.checked_rem_euclid(divisor).unwrap();
    (div, rem)
}

/// Choose even-positioned items from an iterator.
pub fn evens<T>(iter: impl Iterator<Item = T>) -> impl Iterator<Item = T> {
    iter.step_by(2)
}

pub struct Position(pub i16, pub i16);

/// Calculate the manhattan distance of a position from the origin
///
/// # Panics
///
/// If overflow occurs.
impl Position {
    pub fn manhattan(&self) -> i16 {
        let x = self.0.checked_abs().unwrap();
        let y = self.1.checked_abs().unwrap();
        x.checked_add(y).unwrap()
    }
}
