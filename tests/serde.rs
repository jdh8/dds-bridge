//! JSON round-trip tests for serde support.

#![cfg(feature = "serde")]

use dds_bridge::deal::SeatFlags;
use dds_bridge::{
    Bid, Card, Contract, Deal, Hand, Holding, Level, Penalty, Rank, Seat, Strain, Suit,
};

fn roundtrip<T>(value: &T)
where
    T: serde::Serialize + serde::de::DeserializeOwned + core::fmt::Debug + PartialEq,
{
    let json = serde_json::to_string(value).unwrap();
    let parsed: T = serde_json::from_str(&json).unwrap();
    assert_eq!(&parsed, value, "round-trip mismatch for {json}");
}

#[test]
fn strain_json_is_variant_name() {
    assert_eq!(
        serde_json::to_string(&Strain::Spades).unwrap(),
        "\"Spades\""
    );
    assert_eq!(
        serde_json::to_string(&Strain::Notrump).unwrap(),
        "\"Notrump\""
    );
    for &s in &Strain::ASC {
        roundtrip(&s);
    }
}

#[test]
fn suit_json_roundtrip() {
    for &s in &Suit::ASC {
        roundtrip(&s);
    }
}

#[test]
fn seat_json_roundtrip() {
    for &s in &Seat::ALL {
        roundtrip(&s);
    }
}

#[test]
fn rank_json_is_number() {
    assert_eq!(serde_json::to_string(&Rank::A).unwrap(), "14");
    for n in 2..=14 {
        roundtrip(&Rank::new(n));
    }
}

#[test]
fn level_json_is_number() {
    assert_eq!(serde_json::to_string(&Level::new(4)).unwrap(), "4");
    for n in 1..=7 {
        roundtrip(&Level::new(n));
    }
}

#[test]
fn penalty_json_roundtrip() {
    for p in [Penalty::Undoubled, Penalty::Doubled, Penalty::Redoubled] {
        roundtrip(&p);
    }
}

#[test]
fn card_json_is_string() {
    let c = Card {
        suit: Suit::Spades,
        rank: Rank::A,
    };
    assert_eq!(serde_json::to_string(&c).unwrap(), "\"♠A\"");
    roundtrip(&c);
}

#[test]
fn bid_json_is_string() {
    let b = Bid::new(3, Strain::Notrump);
    assert_eq!(serde_json::to_string(&b).unwrap(), "\"3NT\"");
    roundtrip(&b);
}

#[test]
fn contract_json_is_string() {
    let c = Contract::new(4, Strain::Hearts, Penalty::Doubled);
    assert_eq!(serde_json::to_string(&c).unwrap(), "\"4♥x\"");
    roundtrip(&c);
}

#[test]
fn holding_json_is_string() {
    let h: Holding = "AKQJT".parse().unwrap();
    assert_eq!(serde_json::to_string(&h).unwrap(), "\"AKQJT\"");
    roundtrip(&h);
}

#[test]
fn hand_json_is_string() {
    let h: Hand = "AKQJ.T98.765.432".parse().unwrap();
    assert_eq!(serde_json::to_string(&h).unwrap(), "\"AKQJ.T98.765.432\"");
    roundtrip(&h);
}

#[test]
fn deal_json_is_pbn_string() {
    // Each player holds a 13-card straight flush in one suit.
    let d: Deal = "N:AKQJT98765432... .AKQJT98765432.. ..AKQJT98765432. ...AKQJT98765432"
        .parse()
        .unwrap();
    let json = serde_json::to_string(&d).unwrap();
    assert!(json.starts_with("\"N:"), "unexpected: {json}");
    roundtrip(&d);
}

#[test]
fn seat_flags_json_roundtrip() {
    roundtrip(&SeatFlags::NS);
    roundtrip(&SeatFlags::EW);
    roundtrip(&SeatFlags::ALL);
    roundtrip(&(SeatFlags::NORTH | SeatFlags::SOUTH));
}
