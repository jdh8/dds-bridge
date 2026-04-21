//! JSON round-trip tests for serde support.

#![cfg(feature = "serde")]

use dds_bridge::deal::SeatFlags;
use dds_bridge::{
    Bid, Card, Contract, FullDeal, Hand, Holding, Level, Penalty, Rank, Seat, Strain, Suit,
};

fn roundtrip<T>(value: &T) -> serde_json::Result<()>
where
    T: serde::Serialize + serde::de::DeserializeOwned + core::fmt::Debug + PartialEq,
{
    let json = serde_json::to_string(value)?;
    let parsed: T = serde_json::from_str(&json)?;
    assert_eq!(&parsed, value, "round-trip mismatch for {json}");
    Ok(())
}

#[test]
fn strain_json_is_variant_name() -> serde_json::Result<()> {
    assert_eq!(serde_json::to_string(&Strain::Spades)?, "\"Spades\"");
    assert_eq!(serde_json::to_string(&Strain::Notrump)?, "\"Notrump\"");
    for &s in &Strain::ASC {
        roundtrip(&s)?;
    }
    Ok(())
}

#[test]
fn suit_json_roundtrip() -> serde_json::Result<()> {
    for &s in &Suit::ASC {
        roundtrip(&s)?;
    }
    Ok(())
}

#[test]
fn seat_json_roundtrip() -> serde_json::Result<()> {
    for &s in &Seat::ALL {
        roundtrip(&s)?;
    }
    Ok(())
}

#[test]
fn rank_json_is_number() -> serde_json::Result<()> {
    assert_eq!(serde_json::to_string(&Rank::A)?, "14");
    for n in 2..=14 {
        roundtrip(&Rank::new(n))?;
    }
    Ok(())
}

#[test]
fn level_json_is_number() -> serde_json::Result<()> {
    assert_eq!(serde_json::to_string(&Level::new(4))?, "4");
    for n in 1..=7 {
        roundtrip(&Level::new(n))?;
    }
    Ok(())
}

#[test]
fn penalty_json_roundtrip() -> serde_json::Result<()> {
    for p in [Penalty::Undoubled, Penalty::Doubled, Penalty::Redoubled] {
        roundtrip(&p)?;
    }
    Ok(())
}

#[test]
fn card_json_is_string() -> serde_json::Result<()> {
    let c = Card {
        suit: Suit::Spades,
        rank: Rank::A,
    };
    assert_eq!(serde_json::to_string(&c)?, "\"♠A\"");
    roundtrip(&c)
}

#[test]
fn bid_json_is_string() -> serde_json::Result<()> {
    let b = Bid::new(3, Strain::Notrump);
    assert_eq!(serde_json::to_string(&b)?, "\"3NT\"");
    roundtrip(&b)
}

#[test]
fn contract_json_is_string() -> serde_json::Result<()> {
    let c = Contract::new(4, Strain::Hearts, Penalty::Doubled);
    assert_eq!(serde_json::to_string(&c)?, "\"4♥x\"");
    roundtrip(&c)
}

#[test]
fn holding_json_is_string() {
    let h: Holding = "AKQJT".parse().unwrap();
    assert_eq!(serde_json::to_string(&h).unwrap(), "\"AKQJT\"");
    roundtrip(&h).unwrap();
}

#[test]
fn hand_json_is_string() {
    let h: Hand = "AKQJ.T98.765.432".parse().unwrap();
    assert_eq!(serde_json::to_string(&h).unwrap(), "\"AKQJ.T98.765.432\"");
    roundtrip(&h).unwrap();
}

#[test]
fn full_deal_json_is_pbn_string() {
    // Each player holds a 13-card straight flush in one suit.
    let d: FullDeal = "N:AKQJT98765432... .AKQJT98765432.. ..AKQJT98765432. ...AKQJT98765432"
        .parse()
        .unwrap();
    let json = serde_json::to_string(&d).unwrap();
    assert!(json.starts_with("\"N:"), "unexpected: {json}");
    roundtrip(&d).unwrap();
}

#[test]
fn seat_flags_json_roundtrip() -> serde_json::Result<()> {
    roundtrip(&SeatFlags::NS)?;
    roundtrip(&SeatFlags::EW)?;
    roundtrip(&SeatFlags::ALL)?;
    roundtrip(&(SeatFlags::NORTH | SeatFlags::SOUTH))
}
