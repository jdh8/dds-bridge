use super::*;

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
fn test_random_deals() {
    (0..1_000_000).for_each(|_| {
        let hands = Deal::new(&mut rand::rng()).0;
        assert_eq!(hands[0] | hands[1] | hands[2] | hands[3], Hand::ALL);
        assert_eq!(hands[0] & hands[1], Hand::EMPTY);
        assert_eq!(hands[0] & hands[2], Hand::EMPTY);
        assert_eq!(hands[0] & hands[3], Hand::EMPTY);
        assert_eq!(hands[1] & hands[2], Hand::EMPTY);
        assert_eq!(hands[1] & hands[3], Hand::EMPTY);
        assert_eq!(hands[2] & hands[3], Hand::EMPTY);
        assert_eq!(hands[0].len(), 13);
        assert_eq!(hands[1].len(), 13);
        assert_eq!(hands[2].len(), 13);
        assert_eq!(hands[3].len(), 13);
    });
}

#[test]
fn test_ew_shuffling_full_deal() {
    let mut rng = rand::rng();
    let non_trivial_shuffles = core::iter::repeat_with(|| {
        let before = Deal::new(&mut rng);
        let after = before.shuffled(&mut rng, SeatFlags::EW);
        assert_eq!(before[Seat::North], after[Seat::North]);
        assert_eq!(before[Seat::South], after[Seat::South]);
        assert_eq!(after[Seat::East].len(), 13);
        assert_eq!(after[Seat::West].len(), 13);
        assert_eq!(after[Seat::East] & after[Seat::West], Hand::EMPTY);
        assert_eq!(
            before[Seat::East] | before[Seat::West],
            after[Seat::East] | after[Seat::West]
        );
        before[Seat::East] != after[Seat::East]
    });
    assert!(non_trivial_shuffles.take(1_000_000).filter(|&x| x).count() > 0);
}

/// Generate a random deal and remove each card with a 50% chance
///
/// The result is highly unlikely to be a valid deal because of the variance of
/// the number of cards in each hand, but it is useful for testing the
/// [`Deal::shuffled`] method.
fn generate_thanos_deal(rng: &mut (impl rand::Rng + ?Sized)) -> Deal {
    let mut deal = Deal::new(rng);
    deal.0.iter_mut().for_each(|hand| {
        let mask: u64 = rng.random();
        *hand = Hand::from_bits_truncate(hand.to_bits() & mask);
    });
    deal
}

#[test]
fn test_ew_shuffling_thanos_deal() {
    let mut rng = rand::rng();
    let non_trivial_shuffles = core::iter::repeat_with(|| {
        let before = generate_thanos_deal(&mut rng);
        let after = before.shuffled(&mut rng, SeatFlags::EW);
        assert_eq!(before[Seat::North], after[Seat::North]);
        assert_eq!(before[Seat::South], after[Seat::South]);
        assert_eq!(before[Seat::East].len(), after[Seat::East].len());
        assert_eq!(before[Seat::West].len(), after[Seat::West].len());
        assert_eq!(after[Seat::East] & after[Seat::West], Hand::EMPTY);
        assert_eq!(
            before[Seat::East] | before[Seat::West],
            after[Seat::East] | after[Seat::West]
        );
        before[Seat::East] != after[Seat::East]
    });
    assert!(non_trivial_shuffles.take(1_000_000).filter(|&x| x).count() > 0);
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
    const HAND: Hand = Hand([XXX, Holding::EMPTY, XX, Holding::EMPTY]);
    let mut iter = HAND.iter();
    assert_eq!(iter.next(), Some(Card::new(Suit::Clubs, 2)));
    assert_eq!(iter.next(), Some(Card::new(Suit::Clubs, 4)));
    assert_eq!(iter.next(), Some(Card::new(Suit::Clubs, 6)));
    assert_eq!(iter.next(), Some(Card::new(Suit::Hearts, 5)));
    assert_eq!(iter.next(), Some(Card::new(Suit::Hearts, 8)));
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
        Ok(Hand([
            "AK64".parse()?,
            "32".parse()?,
            "QJ9".parse()?,
            "AT74".parse()?,
        ]))
    );

    Ok(())
}

#[test]
fn test_deal_parser() -> Result<(), ParseHandError> {
    let west: Hand = "KQT2.AT.J6542.85".parse()?;
    let east: Hand = "A8654.KQ5.T.QJT6".parse()?;

    assert_eq!(
        "W:KQT2.AT.J6542.85 - A8654.KQ5.T.QJT6 -".parse(),
        Ok(Deal([Hand::EMPTY, east, Hand::EMPTY, west])),
    );

    assert_eq!(
        "N:.63.AKQ987.A9732 A8654.KQ5.T.QJT6 J973.J98742.3.K4 KQT2.AT.J6542.85".parse::<Deal>()?,
        "E:A8654.KQ5.T.QJT6 J973.J98742.3.K4 KQT2.AT.J6542.85 .63.AKQ987.A9732".parse()?,
    );

    Ok(())
}
