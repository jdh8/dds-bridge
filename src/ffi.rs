//! Validation helpers for values returned by DDS through the FFI.
//!
//! Each helper narrows a `c_int` to a bounded bridge-typed value and panics
//! with a descriptive message if DDS returns a value outside the expected
//! range. The panic path is the same as the one described in the solver
//! module's panic policy — reaching it means DDS itself misbehaved.

use contract_bridge::contract::Level;
use contract_bridge::hand::Rank;
use contract_bridge::{Strain, Suit};

use super::tricks::TrickCount;

use core::ffi::c_int;

#[inline]
pub fn trick_count_from_sys(n: c_int) -> TrickCount {
    u8::try_from(n)
        .ok()
        .and_then(|u| TrickCount::try_new(u).ok())
        .unwrap_or_else(|| panic!("DDS returned invalid trick count {n} (expected 0..=13)"))
}

#[inline]
pub fn rank_from_sys(n: c_int) -> Rank {
    u8::try_from(n)
        .ok()
        .and_then(|u| Rank::try_new(u).ok())
        .unwrap_or_else(|| panic!("DDS returned invalid rank {n} (expected 2..=14)"))
}

#[inline]
pub fn level_from_sys(n: c_int) -> Level {
    u8::try_from(n)
        .ok()
        .and_then(|u| Level::try_new(u).ok())
        .unwrap_or_else(|| panic!("DDS returned invalid contract level {n} (expected 1..=7)"))
}

#[inline]
pub fn suit_from_desc_index(i: c_int) -> Suit {
    match i {
        0 => Suit::Spades,
        1 => Suit::Hearts,
        2 => Suit::Diamonds,
        3 => Suit::Clubs,
        _ => panic!("DDS returned invalid suit index {i} (expected 0..=3)"),
    }
}

#[inline]
pub fn strain_from_denom(i: c_int) -> Strain {
    match i {
        0 => Strain::Notrump,
        1 => Strain::Spades,
        2 => Strain::Hearts,
        3 => Strain::Diamonds,
        4 => Strain::Clubs,
        _ => panic!("DDS returned invalid strain index {i} (expected 0..=4)"),
    }
}

/// Validate a `c_int` count field (e.g. `solvedPlay.number`, `futureTricks.cards`).
///
/// Returns the count as `usize`. Panics if the value is negative or exceeds
/// `upper` (the capacity of the corresponding array).
#[inline]
pub fn count_from_sys(n: c_int, upper: usize) -> usize {
    usize::try_from(n)
        .ok()
        .filter(|&u| u <= upper)
        .unwrap_or_else(|| panic!("DDS returned invalid count {n} (expected 0..={upper})"))
}
