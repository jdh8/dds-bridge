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
    T: Copy + Debug + FromStr + PartialEq,
    <T as FromStr>::Err: Debug,
{
    assert_eq!(s.parse::<T>().unwrap(), expected);
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
        Bid::new(1, Clubs),
        Bid::new(7, Notrump),
        Bid::new(3, Hearts),
        Bid::new(2, Spades),
        Bid::new(5, Diamonds),
    ] {
        assert_roundtrip(b);
    }
}

#[test]
fn bid_parses_letter_and_symbol_forms() {
    use Strain::{Clubs, Diamonds, Hearts, Notrump, Spades};

    for (s, expected) in [
        ("1C", Bid::new(1, Clubs)),
        ("1♣", Bid::new(1, Clubs)),
        ("2D", Bid::new(2, Diamonds)),
        ("2♦", Bid::new(2, Diamonds)),
        ("3H", Bid::new(3, Hearts)),
        ("3♥", Bid::new(3, Hearts)),
        ("4S", Bid::new(4, Spades)),
        ("4♠", Bid::new(4, Spades)),
        ("5N", Bid::new(5, Notrump)),
        ("6NT", Bid::new(6, Notrump)),
    ] {
        assert_parses(s, expected);
    }
}

#[test]
fn contract_roundtrip() {
    use Penalty::{Doubled, Redoubled, Undoubled};
    use Strain::{Clubs, Diamonds, Hearts, Notrump, Spades};

    for c in [
        Contract::new(1, Clubs, Undoubled),
        Contract::new(7, Notrump, Redoubled),
        Contract::new(3, Hearts, Doubled),
        Contract::new(2, Spades, Undoubled),
        Contract::new(5, Diamonds, Doubled),
    ] {
        assert_roundtrip(c);
    }
}

#[test]
fn contract_parses_letter_and_symbol_forms() {
    use Penalty::{Doubled, Redoubled, Undoubled};
    use Strain::{Clubs, Hearts, Notrump};

    for (s, expected) in [
        ("1C", Contract::new(1, Clubs, Undoubled)),
        ("1Cx", Contract::new(1, Clubs, Doubled)),
        ("1Cxx", Contract::new(1, Clubs, Redoubled)),
        ("1♣x", Contract::new(1, Clubs, Doubled)),
        ("2NT", Contract::new(2, Notrump, Undoubled)),
        ("2NTx", Contract::new(2, Notrump, Doubled)),
        ("3♥xx", Contract::new(3, Hearts, Redoubled)),
    ] {
        assert_parses(s, expected);
    }
}
