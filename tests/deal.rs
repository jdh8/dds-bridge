use core::num::Wrapping;
use dds_bridge::deal::*;

const _: () = {
    let mut bits = 0;
    while bits < 0xFFFF {
        assert!(Holding::from_bits_truncate(bits).to_bits() == bits & Holding::ALL.to_bits());
        bits += 1;
    }
    assert!(Holding::from_bits_truncate(bits).to_bits() == bits & Holding::ALL.to_bits());
};

fn all_holdings() -> impl Iterator<Item = Holding> {
    (0..1 << 13).map(|i| Holding::from_bits_truncate(i << 2))
}

#[test]
fn test_eq() {
    all_holdings().zip(all_holdings()).for_each(|(u, v)| {
        assert_eq!(u == v, u.to_bits() == v.to_bits());
    });
}

#[test]
fn test_bitand() {
    all_holdings().zip(all_holdings()).for_each(|(u, v)| {
        assert_eq!((u & v).to_bits(), u.to_bits() & v.to_bits());
    });
}

#[test]
fn test_bitor() {
    all_holdings().zip(all_holdings()).for_each(|(u, v)| {
        assert_eq!((u | v).to_bits(), u.to_bits() | v.to_bits());
    });
}

#[test]
fn test_bitxor() {
    all_holdings().zip(all_holdings()).for_each(|(u, v)| {
        assert_eq!((u ^ v).to_bits(), u.to_bits() ^ v.to_bits());
    });
}

#[test]
fn test_sub() {
    all_holdings().zip(all_holdings()).for_each(|(u, v)| {
        assert_eq!((u - v).to_bits(), u.to_bits() & !v.to_bits());
    });
}

#[test]
fn test_not() {
    all_holdings().for_each(|v| {
        assert_eq!(!v, Holding::ALL - v);
        assert_eq!(!v, Holding::ALL ^ v);
    });
}

#[test]
fn test_seat_arithmetics() {
    // Any rotation should work
    const SEATS: [Seat; 4] = [Seat::East, Seat::South, Seat::West, Seat::North];

    (0..4).for_each(|x| {
        (0..4).for_each(|y| {
            assert_eq!(
                SEATS[usize::from(x)] + Wrapping(y),
                SEATS[usize::from(y)] + Wrapping(x)
            );
            assert_eq!(
                SEATS[usize::from(x)] + Wrapping(y),
                SEATS[usize::from((x + y) & 3)]
            );
            assert_eq!(
                SEATS[usize::from(x)] - Wrapping(y),
                SEATS[usize::from((x + 4 - y) & 3)]
            );
        });
    });
}

#[test]
fn test_iter_aqt() {
    const AQT: Holding = Holding::from_bits_truncate(0b10101 << 10);
    let mut iter = AQT.iter();
    assert_eq!(iter.next(), Some(10));
    assert_eq!(iter.next(), Some(12));
    assert_eq!(iter.next(), Some(14));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_iter_spot_cards() {
    const XXX: Holding = Holding::from_bits_truncate(0b10101 << 2);
    const XX: Holding = Holding::from_bits_truncate(0b1001 << 5);
    const HAND: Hand = Hand::new(XXX, Holding::EMPTY, XX, Holding::EMPTY);
    let mut iter = HAND.iter();
    assert_eq!(iter.next(), Card::new(Suit::Clubs, 2).ok());
    assert_eq!(iter.next(), Card::new(Suit::Clubs, 4).ok());
    assert_eq!(iter.next(), Card::new(Suit::Clubs, 6).ok());
    assert_eq!(iter.next(), Card::new(Suit::Hearts, 5).ok());
    assert_eq!(iter.next(), Card::new(Suit::Hearts, 8).ok());
    assert_eq!(iter.next(), None);
}

#[test]
fn test_holding_parser() -> Result<(), ParseHandError> {
    type Err = ParseHandError;
    const AQT: Holding = Holding::from_bits_truncate(0b10101 << 10);
    const KJ32: Holding = Holding::from_bits_truncate(0b0101 << 11 | 0b11 << 2);
    const KJ2: Holding = Holding::from_bits_truncate(0b0101 << 11 | 0b1 << 2);

    assert!(matches!("AKQJT98765432".parse()?, Holding::ALL));
    assert!(matches!("AKQJT987xxxxx".parse()?, Holding::ALL));

    assert!(matches!("AQT".parse()?, AQT));
    assert!(matches!("AQ10".parse()?, AQT));
    assert!(matches!("ATQ".parse::<Holding>(), Err(Err::InvalidHolding)));
    assert!(matches!("KxJ".parse::<Holding>(), Err(Err::InvalidHolding)));

    assert!(matches!("KJ2".parse()?, KJ2));
    assert!(matches!("KJx".parse()?, KJ2));
    assert!(matches!("KJ2x".parse::<Holding>(), Err(Err::RepeatedRank)));

    assert!(matches!("KJ32".parse()?, KJ32));
    assert!(matches!("KJ3x".parse()?, KJ32));
    assert!(matches!("KJ3xx".parse::<Holding>(), Err(Err::RepeatedRank)));

    Ok(())
}

#[test]
fn test_holding_io() -> Result<(), ParseHandError> {
    (0..1 << 13).try_for_each(|bits| {
        let holding = Holding::from_bits_truncate(bits << 2);
        assert_eq!(holding, holding.to_string().parse()?);
        Ok(())
    })
}

#[test]
fn test_hand_parser() -> Result<(), ParseHandError> {
    assert!(matches!("-".parse()?, Hand::EMPTY));
    assert!(matches!("...".parse()?, Hand::EMPTY));

    assert!(matches!(
        "".parse::<Hand>(),
        Err(ParseHandError::NotFourSuits)
    ));

    assert!(matches!(
        ".".parse::<Hand>(),
        Err(ParseHandError::NotFourSuits)
    ));

    assert!(matches!(
        "..".parse::<Hand>(),
        Err(ParseHandError::NotFourSuits)
    ));

    assert!(matches!(
        "....".parse::<Hand>(),
        Err(ParseHandError::NotFourSuits)
    ));

    assert_eq!(
        "AT74.QJ9.32.AK64".parse(),
        Ok(Hand::new(
            "AK64".parse()?,
            "32".parse()?,
            "QJ9".parse()?,
            "AT74".parse()?,
        ))
    );

    Ok(())
}

#[test]
fn test_deal_parser() -> Result<(), ParseHandError> {
    let west: Hand = "KQT2.AT.J6542.85".parse()?;
    let east: Hand = "A8654.KQ5.T.QJT6".parse()?;

    assert_eq!(
        "W:KQT2.AT.J6542.85 - A8654.KQ5.T.QJT6 -".parse(),
        Ok(Deal::new(Hand::EMPTY, east, Hand::EMPTY, west)),
    );

    assert_eq!(
        "N:.63.AKQ987.A9732 A8654.KQ5.T.QJT6 J973.J98742.3.K4 KQT2.AT.J6542.85".parse::<Deal>()?,
        "E:A8654.KQ5.T.QJT6 J973.J98742.3.K4 KQT2.AT.J6542.85 .63.AKQ987.A9732".parse()?,
    );

    Ok(())
}
