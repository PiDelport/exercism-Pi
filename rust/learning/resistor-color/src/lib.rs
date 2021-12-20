//! <https://exercism.org/tracks/rust/exercises/resistor-color>

use enum_iterator::IntoEnumIterator;
use int_enum::IntEnum;

/// Resistor color band values.
#[derive(Clone, Copy, Debug, PartialEq)] // core
#[derive(IntoEnumIterator)] // enum_iterator
#[derive(IntEnum)] // int_enum
#[repr(usize)] // used by int_enum
pub enum ResistorColor {
    Black = 0,
    Brown = 1,
    Red = 2,
    Orange = 3,
    Yellow = 4,
    Green = 5,
    Blue = 6,
    Violet = 7,
    Grey = 8,
    White = 9,
}

/// Look up the numerical value associated with a particular color band.
pub fn color_to_value(color: ResistorColor) -> usize {
    color.int_value()
}

/// Convert the numerical value into a string representing color.
///
/// Return `"value out of range"` for non-decimal-digit values.
pub fn value_to_color_string(value: usize) -> String {
    match ResistorColor::from_int(value) {
        Ok(color) => format!("{:?}", color),
        Err(_) => "value out of range".to_string(),
    }
}

/// List the different band colors.
pub fn colors() -> Vec<ResistorColor> {
    ResistorColor::into_enum_iter().collect()
}
