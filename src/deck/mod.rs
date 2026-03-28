use super::deal::SmallSet as _;
use super::deal::{Card, Deal, Hand, Suit};
use core::mem::MaybeUninit;
use rand::prelude::SliceRandom as _;

/// A deck of [`Card`]s.
///
/// It requires shuffling to partially retrieve cards from the deck.  However,
/// it is deterministic to collect all cards.
pub struct Deck {
    cards: [MaybeUninit<Card>; 52],
    len: usize,
}

/// The standard 52-card deck
pub const STANDARD_52: Deck = {
    let mut cards = [MaybeUninit::uninit(); 52];
    let mut i = 0;

    while i < 52 {
        let index = i as usize;
        cards[index] = MaybeUninit::new(Card::new(Suit::ASC[index & 3], (i >> 2) + 2));
        i += 1;
    }

    Deck { cards, len: 52 }
};

fn collect_cards(cards: &[Card]) -> Hand {
    let mut hand = Hand::default();

    for &card in cards {
        hand.insert(card);
    }
    hand
}

/// Shuffle and evenly deal 52 cards into 4 hands
pub fn full_deal(rng: &mut (impl rand::Rng + ?Sized)) -> Deal {
    let mut deck = STANDARD_52.cards;
    let deck = unsafe { deck.assume_init_mut() };
    let (shuffled, rest) = deck.partial_shuffle(rng, 39);

    Deal([
        collect_cards(rest),
        collect_cards(&shuffled[00..13]),
        collect_cards(&shuffled[13..26]),
        collect_cards(&shuffled[26..39]),
    ])
}
