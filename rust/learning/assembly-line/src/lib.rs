//! <https://exercism.org/tracks/rust/exercises/assembly-line>
//!
//! # Panics
//!
//! All of these functions will panic if `speed` is not in range `0..=10`.

/// The base production rate at speed 1, in cars per hour.
const BASE_PRODUCTION_RATE: f64 = 221.0;

/// The success rate at a given speed, as a factor.
fn success_rate(speed: u8) -> f64 {
    match speed {
        0..=4 => 1.00,
        5..=8 => 0.90,
        9..=10 => 0.77,
        other => panic!("speed out of range: {}", other),
    }
}

/// The number of cars produced in an hour, at the given speed.
pub fn production_rate_per_hour(speed: u8) -> f64 {
    BASE_PRODUCTION_RATE * (speed as f64) * success_rate(speed)
}

/// The number of cars produced in a minute, at the given speed.
pub fn working_items_per_minute(speed: u8) -> u32 {
    (production_rate_per_hour(speed) / 60.0) as u32
}
