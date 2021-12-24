//! <https://exercism.org/tracks/rust/exercises/reverse-string>

use unicode_segmentation::UnicodeSegmentation;

/// Reverse the given string by Unicode grapheme clusters.
///
/// TODO: This is not an involution: some strings change their grapheme clustering when reversed.
pub fn reverse(input: &str) -> String {
    input.graphemes(true).rev().collect()
}
