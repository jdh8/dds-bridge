use crate::deal::{Card, Deal, Hand, Seat, SmallSet as _, Suit};
use core::num::Wrapping;
use rand::prelude::SliceRandom as _;
use rand::{Rng, RngExt as _};
use thiserror::Error;

/// Check optimal memory layout
const _: () = assert!(core::mem::size_of::<Option<Card>>() == core::mem::size_of::<Card>());

/// Error indicating that the deck is already full and cannot accept more cards.
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("The deck is already full")]
pub struct CapacityError;

/// A subset of the standard 52-card deck
///
/// It requires shuffling to partially retrieve cards from the deck.  However,
/// it is deterministic to collect all cards.
#[derive(Debug, Clone)]
pub struct Deck {
    /// The first `len` cards must be `Some`
    cards: [Option<Card>; Self::CAPACITY],
    /// The number of cards currently in the deck
    len: usize,
}

fn force_collect(cards: &[Option<Card>]) -> Hand {
    let mut hand = Hand::EMPTY;

    for &card in cards {
        hand.insert(card.expect("Invalid card in the deck"));
    }
    hand
}

/// Shuffle and evenly deal 52 cards into 4 hands
pub fn full_deal(rng: &mut (impl Rng + ?Sized)) -> Deal {
    let mut deck = Deck::ALL.cards;
    let (shuffled, rest) = deck.partial_shuffle(rng, 39);

    Deal::new(
        force_collect(rest),
        force_collect(&shuffled[00..13]),
        force_collect(&shuffled[13..26]),
        force_collect(&shuffled[26..39]),
    )
}

impl Deck {
    /// The maximum number of cards in a deck
    pub const CAPACITY: usize = 52;

    /// The empty deck
    pub const EMPTY: Self = Self {
        cards: [None; Self::CAPACITY],
        len: 0,
    };

    /// The standard 52-card deck
    pub const ALL: Self = {
        let mut cards = [None; Self::CAPACITY];
        let mut i = 0;

        while (i as usize) < Self::CAPACITY {
            let index = i as usize;
            let Ok(card) = Card::new(Suit::ASC[index & 3], (i >> 2) + 2) else {
                panic!("Invalid card in the standard deck");
            };
            cards[index].replace(card);
            i += 1;
        }
        Self {
            cards,
            len: Self::CAPACITY,
        }
    };

    /// Create a new empty deck
    #[must_use]
    pub const fn new() -> Self {
        Self::EMPTY
    }

    /// The number of cards currently in the deck
    #[must_use]
    pub const fn len(&self) -> usize {
        self.len
    }

    /// Whether the deck is empty
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Clear the deck, removing all the cards.
    pub const fn clear(&mut self) {
        self.len = 0;
    }

    /// Try pushing a card into the deck
    ///
    /// # Errors
    ///
    /// Returns an error if the deck already contains 52 cards.
    pub const fn try_push(&mut self, card: Card) -> Result<(), CapacityError> {
        if self.len >= Self::CAPACITY {
            return Err(CapacityError);
        }
        self.cards[self.len].replace(card);
        self.len += 1;
        Ok(())
    }

    /// Try pushing cards in a hand into the deck
    ///
    /// # Errors
    ///
    /// Returns an error if the resulting deck would contain more than 52 cards.
    pub fn try_extend(&mut self, hand: Hand) -> Result<(), CapacityError> {
        if self.len + hand.len() > Self::CAPACITY {
            return Err(CapacityError);
        }
        for card in hand.iter() {
            self.cards[self.len].replace(card);
            self.len += 1;
        }
        Ok(())
    }

    /// Collect all cards in the deck into a hand.
    #[must_use]
    pub fn collect(&mut self) -> Hand {
        let hand = force_collect(&self.cards[..self.len]);
        self.len = 0;
        hand
    }

    /// Randomly pick `n` cards from the deck and collect them into a hand.
    #[must_use]
    pub fn partial_shuffle(&mut self, rng: &mut (impl Rng + ?Sized), n: usize) -> Hand {
        let hand = force_collect(self.cards[..self.len].partial_shuffle(rng, n).0);
        self.len -= n;
        hand
    }

    /// Randomly pop a card from the deck
    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub fn pop(&mut self, rng: &mut (impl Rng + ?Sized)) -> Option<Card> {
        (self.len > 0).then(|| {
            let index = rng.random_range(0..self.len);
            self.len -= 1;
            self.cards.swap(index, self.len);
            self.cards[self.len].expect("Invalid card in the deck")
        })
    }
}

impl Default for Deck {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Hand> for Deck {
    fn from(hand: Hand) -> Self {
        let mut deck = Self::new();
        deck.try_extend(hand).expect("Hand exceeds deck capacity");
        deck
    }
}

/// Error indicating an invalid deal
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, Hash)]
#[error("The deal is not a valid subset of a bridge deal")]
pub struct InvalidDeal;

/// Given a deal, randomly fill the remaining cards and filter the results.
///
/// The filter is applied before collecting the results, so the resulting vector
/// still has `n` deals.
///
/// - `deal`: The initial deal with some cards already assigned.
/// - `n`: The number of deals to generate.
/// - `filter`: A constraint to filter deals.
///
/// # Errors
///
/// [`InvalidDeal`] if `deal` is invalid determined by
/// [`Deal::validate_and_collect`].
pub fn fill_n_filtered_deals(
    rng: &mut (impl Rng + ?Sized),
    deal: &Deal,
    n: usize,
    filter: impl FnMut(&Deal) -> bool,
) -> Result<Vec<Deal>, InvalidDeal> {
    let deck = Deck::from(deal.validate_and_collect().ok_or(InvalidDeal)?);

    #[allow(clippy::missing_panics_doc)]
    let shortest = Seat::ALL
        .into_iter()
        .min_by_key(|&seat| deal[seat].len())
        .expect("Seat::ALL shall not be empty");

    Ok(core::iter::repeat_with(|| {
        let mut deck = deck.clone();
        let mut deal = *deal;

        for i in 1..=3 {
            let hand = &mut deal[shortest + Wrapping(i)];
            *hand |= deck.partial_shuffle(rng, 13 - hand.len());
        }
        deal[shortest] |= deck.collect();
        deal
    })
    .filter(filter)
    .take(n)
    .collect())
}

/// Given existing cards in a deal, randomly fill the remaining cards.
///
/// - `deal`: The initial deal with some cards already assigned.
/// - `n`: The number of deals to generate.
///
/// # Errors
///
/// [`InvalidDeal`] if `deal` is invalid determined by
/// [`Deal::validate_and_collect`].
pub fn fill_n_deals(
    rng: &mut (impl Rng + ?Sized),
    deal: &Deal,
    n: usize,
) -> Result<Vec<Deal>, InvalidDeal> {
    fill_n_filtered_deals(rng, deal, n, |_| true)
}
