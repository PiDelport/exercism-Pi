//! <https://exercism.org/tracks/rust/exercises/magazine-cutout>

use std::collections::HashMap;
use std::hash::Hash;

/// A counter (multiset or bag) of items, implemented as a map from items to counts.
pub type Counter<Item> = HashMap<Item, usize>;

/// Construct a [`Counter`] from the given items.
///
/// The `Item` type may be any target of [`ToOwned`], such as [`&str`] or [`String`].
///
/// # Panics
///
/// If any item counter overflows.
///
/// # Examples
///
/// ```
/// # use magazine_cutout::count;
///
/// let counter = count(&["a", "c", "a", "b"]);
/// assert_eq!(counter.get("a"), Some(&2));
/// assert_eq!(counter.get("b"), Some(&1));
/// assert_eq!(counter.get("c"), Some(&1));
/// ```
pub fn count<Item>(items: &[Item]) -> Counter<Item>
where
    Item: Eq + Hash + ToOwned<Owned = Item>,
{
    let mut counts = Counter::new();
    for item in items {
        let count = counts.entry(item.to_owned()).or_default();
        *count = count.checked_add(1).expect("counter overflowed");
    }
    counts
}

/// True if `big` includes all items in `small`.
fn includes<Item>(big: Counter<Item>, small: Counter<Item>) -> bool
where
    Item: Eq + Hash,
{
    small.iter().all(|(k, v)| v <= big.get(k).unwrap_or(&0))
}

/// True if `magazine` contains all the words in `note`.
pub fn can_construct_note(magazine: &[&str], note: &[&str]) -> bool {
    includes(count(magazine), count(note))
}
