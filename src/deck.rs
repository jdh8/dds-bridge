use crate::Suit;
use crate::deal::{Card, Deal, Hand, Rank, Seat, SmallSet as _};
use arrayvec::{ArrayVec, CapacityError};
use rand::prelude::SliceRandom as _;
use rand::{Rng, RngExt as _};
use thiserror::Error;

/// Error while generating deals
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    /// The deck is already full and cannot accept more cards.
    #[error("{0}")]
    Capacity(CapacityError<Card>),
    /// The deal is not a valid subset of a bridge deal
    #[error("The deal is not a valid subset of a bridge deal")]
    Invalid,
}

impl From<CapacityError<Card>> for Error {
    fn from(value: CapacityError<Card>) -> Self {
        Self::Capacity(value)
    }
}

/// A subset of the standard 52-card deck
///
/// It requires shuffling to partially retrieve cards from the deck.  However,
/// it is deterministic to collect all cards.
#[derive(Debug, Clone)]
pub struct Deck {
    cards: ArrayVec<Card, 52>,
}

/// Shuffle and evenly deal 52 cards into 4 hands
pub fn full_deal(rng: &mut (impl Rng + ?Sized)) -> Deal {
    let mut deck = Deck::standard_52().cards;
    let (shuffled, rest) = deck.partial_shuffle(rng, 39);

    Deal::new(
        rest.iter().copied().collect(),
        shuffled[00..13].iter().copied().collect(),
        shuffled[13..26].iter().copied().collect(),
        shuffled[26..39].iter().copied().collect(),
    )
}

impl Deck {
    /// The maximum number of cards in a deck
    pub const CAPACITY: usize = 52;

    /// The standard 52-card deck
    #[must_use]
    pub fn standard_52() -> Self {
        let mut cards = ArrayVec::new();
        for i in 0..Self::CAPACITY {
            // SAFETY: `i` < `Self::CAPACITY` = 52, so `(i >> 2) as u8 + 2` is
            // in the range [2, 14], which is valid for `Rank`.
            #[allow(clippy::cast_possible_truncation)]
            let rank = Rank::new((i >> 2) as u8 + 2);
            cards.push(Card::new(Suit::ASC[i & 3], rank));
        }
        Self { cards }
    }

    /// Create a new empty deck
    #[must_use]
    pub const fn new() -> Self {
        Self {
            cards: ArrayVec::new_const(),
        }
    }

    /// The number of cards currently in the deck
    #[must_use]
    pub const fn len(&self) -> usize {
        self.cards.len()
    }

    /// Whether the deck is empty
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.cards.is_empty()
    }

    /// Clear the deck, removing all the cards.
    pub fn clear(&mut self) {
        self.cards.clear();
    }

    /// Try pushing a card into the deck
    ///
    /// # Errors
    ///
    /// [`CapacityError`] if the deck already contains 52 cards.
    pub fn try_push(&mut self, card: Card) -> Result<(), CapacityError<Card>> {
        self.cards.try_push(card)
    }

    /// Try pushing cards in a hand into the deck
    ///
    /// # Errors
    ///
    /// [`CapacityError`] if the resulting deck would contain more than 52 cards.
    pub fn try_extend(&mut self, hand: Hand) -> Result<(), CapacityError<Card>> {
        for card in hand.iter() {
            self.cards.try_push(card)?;
        }
        Ok(())
    }

    /// Drain the remaining cards in the deck into a hand.
    #[must_use]
    pub fn drain(&mut self) -> Hand {
        self.cards.drain(..).collect()
    }

    /// Randomly pick `n` cards from the deck and collect them into a hand.
    #[must_use]
    pub fn partial_shuffle(&mut self, rng: &mut (impl Rng + ?Sized), n: usize) -> Hand {
        self.cards.partial_shuffle(rng, n);
        self.cards.drain(self.cards.len() - n..).collect()
    }

    /// Randomly pop a card from the deck
    #[must_use]
    pub fn pop(&mut self, rng: &mut (impl Rng + ?Sized)) -> Option<Card> {
        match self.cards.len() {
            0..=1 => self.cards.pop(),
            len => self.cards.swap_pop(rng.random_range(0..len)),
        }
    }
}

impl Default for Deck {
    fn default() -> Self {
        Self::new()
    }
}

impl TryFrom<Hand> for Deck {
    type Error = CapacityError<Card>;

    fn try_from(hand: Hand) -> Result<Self, Self::Error> {
        let mut deck = Self::new();
        deck.try_extend(hand)?;
        Ok(deck)
    }
}

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
/// [`Error::Invalid`] if `deal` is invalid determined by
/// [`Deal::validate_and_collect`].
pub fn fill_n_filtered_deals(
    rng: &mut (impl Rng + ?Sized),
    deal: &Deal,
    n: usize,
    filter: impl FnMut(&Deal) -> bool,
) -> Result<Vec<Deal>, Error> {
    let deck = Deck::try_from(deal.validate_and_collect().ok_or(Error::Invalid)?)?;

    #[allow(clippy::missing_panics_doc)]
    let shortest = Seat::ALL
        .into_iter()
        .min_by_key(|&seat| deal[seat].len())
        .expect("Seat::ALL shall not be empty");

    Ok(core::iter::repeat_with(|| {
        let mut deck = deck.clone();
        let mut deal = *deal;
        let mut fill = |hand: &mut Hand| *hand |= deck.partial_shuffle(rng, 13 - hand.len());

        fill(&mut deal[shortest.lho()]);
        fill(&mut deal[shortest.partner()]);
        fill(&mut deal[shortest.rho()]);

        deal[shortest] |= deck.drain();
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
/// [`Error::Invalid`] if `deal` is invalid determined by
/// [`Deal::validate_and_collect`].
pub fn fill_n_deals(
    rng: &mut (impl Rng + ?Sized),
    deal: &Deal,
    n: usize,
) -> Result<Vec<Deal>, Error> {
    fill_n_filtered_deals(rng, deal, n, |_| true)
}
