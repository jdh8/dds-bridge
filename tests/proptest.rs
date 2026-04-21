//! Round-trip property tests for `Display` / `FromStr` on the public types.

use dds_bridge::{
    Bid, Builder, Card, Contract, FullDeal, Hand, Holding, Level, Penalty, Rank, Seat, Strain,
    Subset,
};
use proptest::prelude::*;

fn rank() -> impl Strategy<Value = Rank> {
    (2u8..=14).prop_map(Rank::new)
}

fn level() -> impl Strategy<Value = Level> {
    (1u8..=7).prop_map(Level::new)
}

fn strain() -> impl Strategy<Value = Strain> {
    (0usize..Strain::ASC.len()).prop_map(|i| Strain::ASC[i])
}

fn penalty() -> impl Strategy<Value = Penalty> {
    prop_oneof![
        Just(Penalty::Undoubled),
        Just(Penalty::Doubled),
        Just(Penalty::Redoubled),
    ]
}

fn seat() -> impl Strategy<Value = Seat> {
    (0usize..Seat::ALL.len()).prop_map(|i| Seat::ALL[i])
}

fn card() -> impl Strategy<Value = Card> {
    (0usize..dds_bridge::Suit::ASC.len(), rank()).prop_map(|(i, rank)| Card {
        suit: dds_bridge::Suit::ASC[i],
        rank,
    })
}

fn bid() -> impl Strategy<Value = Bid> {
    (level(), strain()).prop_map(|(level, strain)| Bid { level, strain })
}

fn contract() -> impl Strategy<Value = Contract> {
    (bid(), penalty()).prop_map(|(bid, penalty)| Contract { bid, penalty })
}

fn holding() -> impl Strategy<Value = Holding> {
    (0u16..=Holding::ALL.to_bits()).prop_map(Holding::from_bits_truncate)
}

fn hand() -> impl Strategy<Value = Hand> {
    [holding(), holding(), holding(), holding()].prop_map(|[c, d, h, s]| {
        let mut hand = Hand::EMPTY;
        hand[dds_bridge::Suit::Clubs] = c;
        hand[dds_bridge::Suit::Diamonds] = d;
        hand[dds_bridge::Suit::Hearts] = h;
        hand[dds_bridge::Suit::Spades] = s;
        hand
    })
}

fn all_cards() -> impl Iterator<Item = Card> {
    dds_bridge::Suit::ASC.into_iter().flat_map(|suit| {
        (2u8..=14).map(move |rank| Card {
            suit,
            rank: Rank::new(rank),
        })
    })
}

/// Shuffle the 52-card deck deterministically from a seed, deal the first
/// 13 cards to North, the next 13 to East, and so on.
fn full_deal() -> impl Strategy<Value = FullDeal> {
    any::<[u8; 52]>().prop_map(|seed| {
        let mut deck: Vec<Card> = all_cards().collect();
        for i in (1..52).rev() {
            let j = (seed[i - 1] as usize) % (i + 1);
            deck.swap(i, j);
        }
        let collect_hand = |slice: &[Card]| slice.iter().copied().collect::<Hand>();
        Builder::new()
            .north(collect_hand(&deck[0..13]))
            .east(collect_hand(&deck[13..26]))
            .south(collect_hand(&deck[26..39]))
            .west(collect_hand(&deck[39..52]))
            .build_full()
            .unwrap()
    })
}

/// Assign each of the 52 cards to one of 5 buckets: North, East, South, West,
/// or "not dealt".  A card is dropped if its target hand already holds 13
/// cards, so the subset invariant holds (≤13 per hand, pairwise disjoint).
fn subset() -> impl Strategy<Value = Subset> {
    any::<[u8; 52]>().prop_map(|seed| {
        let mut hands = [Hand::EMPTY; 4];
        for (i, card) in all_cards().enumerate() {
            let bucket = (seed[i] as usize) % 5;
            if bucket < 4 && hands[bucket].len() < 13 {
                hands[bucket].insert(card);
            }
        }
        Builder::new()
            .north(hands[0])
            .east(hands[1])
            .south(hands[2])
            .west(hands[3])
            .build_subset()
            .unwrap()
    })
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(256))]

    #[test]
    fn rank_roundtrip(r in rank()) {
        prop_assert_eq!(r.to_string().parse::<Rank>(), Ok(r));
    }

    #[test]
    fn level_roundtrip(l in level()) {
        prop_assert_eq!(l.to_string().parse::<Level>(), Ok(l));
    }

    #[test]
    fn strain_roundtrip(s in strain()) {
        prop_assert_eq!(s.to_string().parse::<Strain>(), Ok(s));
    }

    #[test]
    fn penalty_roundtrip(p in penalty()) {
        prop_assert_eq!(p.to_string().parse::<Penalty>(), Ok(p));
    }

    #[test]
    fn card_roundtrip(c in card()) {
        prop_assert_eq!(c.to_string().parse::<Card>(), Ok(c));
    }

    #[test]
    fn bid_roundtrip(b in bid()) {
        prop_assert_eq!(b.to_string().parse::<Bid>(), Ok(b));
    }

    #[test]
    fn contract_roundtrip(c in contract()) {
        prop_assert_eq!(c.to_string().parse::<Contract>(), Ok(c));
    }

    #[test]
    fn holding_roundtrip(h in holding()) {
        prop_assert_eq!(h.to_string().parse::<Holding>(), Ok(h));
    }

    #[test]
    fn hand_roundtrip(h in hand()) {
        prop_assert_eq!(h.to_string().parse::<Hand>(), Ok(h));
    }

    #[test]
    fn full_deal_roundtrip(d in full_deal(), s in seat()) {
        prop_assert_eq!(d.display(s).to_string().parse::<FullDeal>(), Ok(d));
    }

    #[test]
    fn subset_roundtrip(d in subset(), s in seat()) {
        prop_assert_eq!(d.display(s).to_string().parse::<Subset>(), Ok(d));
    }
}
