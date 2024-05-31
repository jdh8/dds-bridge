#[cfg(test)]
mod test;

use crate::contract::Strain;
use core::fmt;
use core::ops::{BitAnd, BitOr, BitXor, Index, IndexMut, Not, Sub};
use rand::prelude::SliceRandom as _;

/// Position at the table
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Seat {
    /// Dealer of Board 1, partner of [`Seat::South`]
    North,
    /// Dealer of Board 2, partner of [`Seat::West`]
    East,
    /// Dealer of Board 3, partner of [`Seat::North`]
    South,
    /// Dealer of Board 4, partner of [`Seat::East`]
    West,
}

/// A playing card
#[derive(Debug, Clone, Copy)]
pub struct Card {
    /// The suit of the card
    pub suit: Strain,

    /// The rank of the card
    ///
    /// The rank is a number from 2 to 14.  J, Q, K, A are denoted as 11, 12,
    /// 13, 14 respectively.
    pub rank: u8,
}

impl Card {
    /// Create a card from suit and rank
    #[must_use]
    pub const fn new(suit: Strain, rank: u8) -> Self {
        Self { suit, rank }
    }
}

/// A bitset whose size is known at compile time
pub trait SmallSet<T>: Copy + Eq + BitAnd + BitOr + BitXor + Not + Sub {
    /// The empty set
    const EMPTY: Self;

    /// The set containing all possible values
    const ALL: Self;

    /// The number of elements in the set
    #[must_use]
    fn len(self) -> usize;

    /// Whether the set is empty
    #[must_use]
    fn is_empty(self) -> bool {
        self == Self::EMPTY
    }

    /// Whether the set contains a value
    fn contains(self, value: T) -> bool;

    /// Insert a value into the set
    fn insert(&mut self, value: T) -> bool;

    /// Remove a value from the set
    fn remove(&mut self, value: T) -> bool;

    /// Toggle a value in the set
    fn toggle(&mut self, value: T) -> bool;
}

/// Holding for a suit in a hand
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Holding(u16);

impl SmallSet<u8> for Holding {
    const EMPTY: Self = Self(0);
    const ALL: Self = Self(0x7FFC);

    fn len(self) -> usize {
        self.0.count_ones() as usize
    }

    fn contains(self, rank: u8) -> bool {
        self.0 & 1 << rank != 0
    }

    fn insert(&mut self, rank: u8) -> bool {
        let insertion = 1 << rank & Self::ALL.0;
        let inserted = insertion & !self.0 != 0;
        self.0 |= insertion;
        inserted
    }

    fn remove(&mut self, rank: u8) -> bool {
        let removed = self.contains(rank);
        self.0 &= !(1 << rank);
        removed
    }

    fn toggle(&mut self, rank: u8) -> bool {
        self.0 ^= 1 << rank & Self::ALL.0;
        self.contains(rank)
    }
}

impl Holding {
    /// As a bitset of ranks
    #[must_use]
    pub const fn to_bits(self) -> u16 {
        self.0
    }

    /// Create a holding from a bitset of ranks
    #[must_use]
    pub const fn from_bits(bits: u16) -> Self {
        Self(bits & Self::ALL.0)
    }
}

impl BitAnd for Holding {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }
}

impl BitOr for Holding {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

impl BitXor for Holding {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self {
        Self(self.0 ^ rhs.0)
    }
}

impl Not for Holding {
    type Output = Self;

    fn not(self) -> Self {
        Self::ALL ^ self
    }
}

impl Sub for Holding {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        self & !rhs
    }
}

impl fmt::Display for Holding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for rank in (2..15).rev() {
            if self.contains(rank) {
                use fmt::Write;
                f.write_char(b"23456789TJQKA"[rank as usize - 2] as char)?;
            }
        }
        Ok(())
    }
}

/// A hand of playing cards
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Hand([Holding; 4]);

impl Index<Strain> for Hand {
    type Output = Holding;

    fn index(&self, suit: Strain) -> &Holding {
        &self.0[suit as usize]
    }
}

impl IndexMut<Strain> for Hand {
    fn index_mut(&mut self, suit: Strain) -> &mut Holding {
        &mut self.0[suit as usize]
    }
}

impl Hand {
    /// As a bitset of cards
    #[must_use]
    pub const fn to_bits(self) -> u64 {
        unsafe { core::mem::transmute(self.0) }
    }

    /// Create a hand from a bitset of cards
    ///
    /// This function removes invalid cards.
    #[must_use]
    pub const fn from_bits(bits: u64) -> Self {
        Self(unsafe { core::mem::transmute(bits & Self::ALL.to_bits()) })
    }

    /// Create a hand from a bitset of cards without checking
    ///
    /// # Safety
    /// The bitset must not contain invalid cards.
    #[must_use]
    pub const unsafe fn from_bits_unchecked(bits: u64) -> Self {
        Self(core::mem::transmute(bits))
    }
}

impl SmallSet<Card> for Hand {
    const EMPTY: Self = Self([Holding::EMPTY; 4]);
    const ALL: Self = Self([Holding::ALL; 4]);

    fn len(self) -> usize {
        self.to_bits().count_ones() as usize
    }

    fn contains(self, card: Card) -> bool {
        self[card.suit].contains(card.rank)
    }

    fn insert(&mut self, card: Card) -> bool {
        self[card.suit].insert(card.rank)
    }

    fn remove(&mut self, card: Card) -> bool {
        self[card.suit].remove(card.rank)
    }

    fn toggle(&mut self, card: Card) -> bool {
        self[card.suit].toggle(card.rank)
    }
}

impl fmt::Display for Hand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}.{}.{}.{}",
            self[Strain::Spades],
            self[Strain::Hearts],
            self[Strain::Diamonds],
            self[Strain::Clubs]
        )
    }
}

impl BitAnd for Hand {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        // SAFETY: safe when both operands are valid
        unsafe { Self::from_bits_unchecked(self.to_bits() & rhs.to_bits()) }
    }
}

impl BitOr for Hand {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        // SAFETY: safe when both operands are valid
        unsafe { Self::from_bits_unchecked(self.to_bits() | rhs.to_bits()) }
    }
}

impl BitXor for Hand {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self {
        // SAFETY: safe when both operands are valid
        unsafe { Self::from_bits_unchecked(self.to_bits() ^ rhs.to_bits()) }
    }
}

impl Not for Hand {
    type Output = Self;

    fn not(self) -> Self {
        Self::ALL ^ self
    }
}

impl Sub for Hand {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        self & !rhs
    }
}

/// A deal of four hands
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Deal([Hand; 4]);

impl Index<Seat> for Deal {
    type Output = Hand;

    fn index(&self, seat: Seat) -> &Hand {
        &self.0[seat as usize]
    }
}

impl IndexMut<Seat> for Deal {
    fn index_mut(&mut self, seat: Seat) -> &mut Hand {
        &mut self.0[seat as usize]
    }
}

impl fmt::Display for Deal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "N:{} {} {} {}",
            self[Seat::North],
            self[Seat::East],
            self[Seat::South],
            self[Seat::West]
        )
    }
}

/// A deck of playing cards
#[derive(Debug, Clone, Default)]
struct Deck {
    /// The cards in the deck
    pub cards: Vec<Card>,
}

impl Deck {
    /// Create a standard 52-card deck
    #[must_use]
    fn standard_52() -> Self {
        Self {
            cards: Strain::SUITS
                .into_iter()
                .flat_map(|x| core::iter::repeat(x).zip(2..=14))
                .map(|(suit, rank)| Card::new(suit, rank))
                .collect(),
        }
    }

    /// Deal the deck into four hands
    #[must_use]
    fn deal(&self) -> Deal {
        let mut deal = Deal::default();

        for (index, card) in self.cards.iter().enumerate() {
            #[allow(clippy::cast_possible_truncation)]
            deal[unsafe { core::mem::transmute((index & 0x3) as u8) }].insert(*card);
        }

        deal
    }

    /// Shuffle the deck
    fn shuffle(&mut self, rng: &mut (impl rand::Rng + ?Sized)) {
        self.cards.shuffle(rng);
    }
}

impl Deal {
    /// Create a deal from a shuffled standard 52-card deck
    pub fn new(rng: &mut (impl rand::Rng + ?Sized)) -> Self {
        let mut deck = Deck::standard_52();
        deck.shuffle(rng);
        deck.deal()
    }
}
