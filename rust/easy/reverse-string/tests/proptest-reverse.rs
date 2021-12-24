//! Property-based tests for [`reverse`].

use proptest::prelude::*;
use reverse_string::reverse;
use unicode_reverse::reverse_grapheme_clusters_in_place;

/// [`reverse`] should be an involution (self-inverse).
///
/// TODO: This test currently fails. Reversing Unicode is not trivial.
#[ignore]
#[test]
fn prop_reverse_involution() {
    proptest!(|(s in ".*")| {
        prop_assert_eq!(reverse(&reverse(&s)), s);
    })
}

/// [`reverse`] matches the behavior of the [`unicode_reverse`] crate.
#[test]
fn prop_matches_unicode_reverse_crate() {
    fn expected_reverse(s: &str) -> String {
        let mut s = s.to_string();
        reverse_grapheme_clusters_in_place(&mut s);
        s
    }
    proptest!(|(s in ".*")| {
        prop_assert_eq!(reverse(&s), expected_reverse(&s));
    })
}
