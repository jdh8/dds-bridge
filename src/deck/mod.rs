use super::deal::{Card, Deal, Hand, SmallSet as _, Suit};
use core::mem::MaybeUninit;
use rand::Rng;
use rand::prelude::SliceRandom as _;
use thiserror::Error;

/// Error indicating that the deck is already full and cannot accept more cards.
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("The deck is already full")]
pub struct CapacityError;

/// A subset of the standard 52-card deck
///
/// It requires shuffling to partially retrieve cards from the deck.  However,
/// it is deterministic to collect all cards.
pub struct Deck {
    cards: [MaybeUninit<Card>; 52],
    len: usize,
}

fn collect_cards(cards: &[Card]) -> Hand {
    let mut hand = Hand::EMPTY;

    for &card in cards {
        hand.insert(card);
    }
    hand
}

/// Shuffle and evenly deal 52 cards into 4 hands
pub fn full_deal(rng: &mut (impl Rng + ?Sized)) -> Deal {
    let mut deck = Deck::ALL.cards;
    let deck = unsafe { deck.assume_init_mut() };
    let (shuffled, rest) = deck.partial_shuffle(rng, 39);

    Deal([
        collect_cards(rest),
        collect_cards(&shuffled[00..13]),
        collect_cards(&shuffled[13..26]),
        collect_cards(&shuffled[26..39]),
    ])
}

impl Deck {
    /// The empty deck
    pub const EMPTY: Self = Self {
        cards: [MaybeUninit::uninit(); 52],
        len: 0,
    };

    /// The standard 52-card deck
    pub const ALL: Self = {
        let mut cards = [MaybeUninit::uninit(); 52];
        let mut i = 0;

        while i < 52 {
            let index = i as usize;
            cards[index] = MaybeUninit::new(Card::new(Suit::ASC[index & 3], (i >> 2) + 2));
            i += 1;
        }
        Self { cards, len: 52 }
    };

    /// Try pushing a card into the deck
    ///
    /// # Errors
    ///
    /// Returns an error if the deck already contains 52 cards.
    pub const fn try_push(&mut self, card: Card) -> Result<(), CapacityError> {
        if self.len >= 52 {
            return Err(CapacityError);
        }
        self.cards[self.len] = MaybeUninit::new(card);
        self.len += 1;
        Ok(())
    }

    /// Try pushing cards in a hand into the deck
    ///
    /// # Errors
    ///
    /// Returns an error if the resulting deck would contain more than 52 cards.
    pub fn try_extend(&mut self, hand: Hand) -> Result<(), CapacityError> {
        if self.len + hand.len() > 52 {
            return Err(CapacityError);
        }
        for card in hand.iter() {
            self.cards[self.len] = MaybeUninit::new(card);
            self.len += 1;
        }
        Ok(())
    }

    /// Collect all cards in the deck into a hand.
    #[must_use]
    pub fn collect(&mut self) -> Hand {
        let hand = collect_cards(unsafe { self.cards[..self.len].assume_init_ref() });
        self.len = 0;
        hand
    }

    /// Randomly pick `n` cards from the deck and collect them into a hand.
    #[must_use]
    pub fn partial_shuffle(&mut self, rng: &mut (impl Rng + ?Sized), n: usize) -> Hand {
        let cards = unsafe { self.cards[..self.len].assume_init_mut() };
        let hand = collect_cards(cards.partial_shuffle(rng, n).0);
        self.len -= n;
        hand
    }
}
