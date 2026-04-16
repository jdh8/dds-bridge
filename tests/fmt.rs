use core::fmt::{Debug, Display};
use core::str::FromStr;

use dds_bridge::{Bid, Contract, Level, Penalty, Strain};

/// Assert that `value` survives a `Display` → `FromStr` round-trip.
fn assert_roundtrip<T>(value: T)
where
    T: Copy + Debug + Display + FromStr + PartialEq,
    <T as FromStr>::Err: Debug,
{
    assert_eq!(value.to_string().parse::<T>().unwrap(), value);
}

/// Assert that `s` parses to `expected`.
fn assert_parses<T>(s: &str, expected: T)
where
    T: Debug + FromStr + PartialEq,
    <T as FromStr>::Err: Debug,
{
    assert_eq!(s.parse::<T>().unwrap(), expected);
}

const fn bid(level: u8, strain: Strain) -> Bid {
    Bid::new(Level::new(level), strain)
}

const fn contract(level: u8, strain: Strain, penalty: Penalty) -> Contract {
    Contract {
        bid: bid(level, strain),
        penalty,
    }
}

#[test]
fn level_roundtrip() {
    for n in 1..=7 {
        assert_roundtrip(Level::new(n));
    }
}

#[test]
fn strain_roundtrip() {
    for strain in Strain::ASC {
        assert_roundtrip(strain);
    }
}

#[test]
fn penalty_display_and_parse() {
    for (penalty, s) in [
        (Penalty::Undoubled, ""),
        (Penalty::Doubled, "x"),
        (Penalty::Redoubled, "xx"),
    ] {
        assert_eq!(penalty.to_string(), s);
        assert_parses(s, penalty);
    }
}

#[test]
fn penalty_parse_rejects_garbage() {
    for s in ["xxx", "X", "Xx", "xX", "XxX", " ", "y"] {
        assert!(s.parse::<Penalty>().is_err());
    }
}

#[test]
fn bid_roundtrip() {
    use Strain::{Clubs, Diamonds, Hearts, Notrump, Spades};

    for b in [
        bid(1, Clubs),
        bid(7, Notrump),
        bid(3, Hearts),
        bid(2, Spades),
        bid(5, Diamonds),
    ] {
        assert_roundtrip(b);
    }
}

#[test]
fn bid_parses_letter_and_symbol_forms() {
    use Strain::{Clubs, Diamonds, Hearts, Notrump, Spades};

    for (s, expected) in [
        ("1C", bid(1, Clubs)),
        ("1♣", bid(1, Clubs)),
        ("2D", bid(2, Diamonds)),
        ("2♦", bid(2, Diamonds)),
        ("3H", bid(3, Hearts)),
        ("3♥", bid(3, Hearts)),
        ("4S", bid(4, Spades)),
        ("4♠", bid(4, Spades)),
        ("5N", bid(5, Notrump)),
        ("6NT", bid(6, Notrump)),
    ] {
        assert_parses(s, expected);
    }
}

#[test]
fn contract_roundtrip() {
    use Penalty::{Doubled, Redoubled, Undoubled};
    use Strain::{Clubs, Diamonds, Hearts, Notrump, Spades};

    for c in [
        contract(1, Clubs, Undoubled),
        contract(7, Notrump, Redoubled),
        contract(3, Hearts, Doubled),
        contract(2, Spades, Undoubled),
        contract(5, Diamonds, Doubled),
    ] {
        assert_roundtrip(c);
    }
}

#[test]
fn contract_parses_letter_and_symbol_forms() {
    use Penalty::{Doubled, Redoubled, Undoubled};
    use Strain::{Clubs, Hearts, Notrump};

    for (s, expected) in [
        ("1C", contract(1, Clubs, Undoubled)),
        ("1Cx", contract(1, Clubs, Doubled)),
        ("1Cxx", contract(1, Clubs, Redoubled)),
        ("1♣x", contract(1, Clubs, Doubled)),
        ("2NT", contract(2, Notrump, Undoubled)),
        ("2NTx", contract(2, Notrump, Doubled)),
        ("3♥xx", contract(3, Hearts, Redoubled)),
    ] {
        assert_parses(s, expected);
    }
}
