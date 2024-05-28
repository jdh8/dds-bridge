use crate::contract::Strain;
use bitflags::bitflags;
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
pub trait SmallSet<T>: Copy + PartialEq + BitAnd + BitOr + BitXor + Not + Sub {
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
    pub const fn bits(self) -> u16 {
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
        Self::from_bits(!self.0)
    }
}

impl Sub for Holding {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Self(self.0 & !rhs.0)
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
pub struct Hand(Holding, Holding, Holding, Holding);

impl Index<Strain> for Hand {
    type Output = Holding;

    fn index(&self, suit: Strain) -> &Holding {
        match suit {
            Strain::Clubs => &self.0,
            Strain::Diamonds => &self.1,
            Strain::Hearts => &self.2,
            Strain::Spades => &self.3,
            Strain::Notrump => panic!("Notrump is not a suit"),
        }
    }
}

impl IndexMut<Strain> for Hand {
    fn index_mut(&mut self, suit: Strain) -> &mut Holding {
        match suit {
            Strain::Clubs => &mut self.0,
            Strain::Diamonds => &mut self.1,
            Strain::Hearts => &mut self.2,
            Strain::Spades => &mut self.3,
            Strain::Notrump => panic!("Notrump is not a suit"),
        }
    }
}

impl SmallSet<Card> for Hand {
    const EMPTY: Self = Self(
        Holding::EMPTY,
        Holding::EMPTY,
        Holding::EMPTY,
        Holding::EMPTY,
    );

    const ALL: Self = Self(Holding::ALL, Holding::ALL, Holding::ALL, Holding::ALL);

    fn len(self) -> usize {
        self.0.len() + self.1.len() + self.2.len() + self.3.len()
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
        Self(
            self.0 & rhs.0,
            self.1 & rhs.1,
            self.2 & rhs.2,
            self.3 & rhs.3,
        )
    }
}

impl BitOr for Hand {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self(
            self.0 | rhs.0,
            self.1 | rhs.1,
            self.2 | rhs.2,
            self.3 | rhs.3,
        )
    }
}

impl BitXor for Hand {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self {
        Self(
            self.0 ^ rhs.0,
            self.1 ^ rhs.1,
            self.2 ^ rhs.2,
            self.3 ^ rhs.3,
        )
    }
}

impl Not for Hand {
    type Output = Self;

    fn not(self) -> Self {
        Self(!self.0, !self.1, !self.2, !self.3)
    }
}

impl Sub for Hand {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Self(
            self.0 - rhs.0,
            self.1 - rhs.1,
            self.2 - rhs.2,
            self.3 - rhs.3,
        )
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

bitflags! {
    /// Flags for the solver to solve for a strain
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct StrainFlags : u8 {
        /// Solve for clubs ([`Strain::Clubs`])
        const CLUBS = 0x01;
        /// Solve for diamonds ([`Strain::Diamonds`])
        const DIAMONDS = 0x02;
        /// Solve for hearts ([`Strain::Hearts`])
        const HEARTS = 0x04;
        /// Solve for spades ([`Strain::Spades`])
        const SPADES = 0x08;
        /// Solve for notrump ([`Strain::Notrump`])
        const NOTRUMP = 0x10;
    }
}

/// A deck of playing cards
#[derive(Debug, Clone, Default)]
pub struct Deck {
    /// The cards in the deck
    pub cards: Vec<Card>,
}

impl Deck {
    /// Create a standard 52-card deck
    #[must_use]
    pub fn standard_52() -> Self {
        let suits = [
            Strain::Clubs,
            Strain::Diamonds,
            Strain::Hearts,
            Strain::Spades,
        ];
        let product = suits.iter().flat_map(|x| core::iter::repeat(x).zip(2..15));
        Self {
            cards: product.map(|(suit, rank)| Card::new(*suit, rank)).collect(),
        }
    }

    /// Deal the deck into four hands
    #[must_use]
    pub fn deal(&self) -> Deal {
        let mut deal = Deal::default();

        for (index, card) in self.cards.iter().enumerate() {
            #[allow(clippy::cast_possible_truncation)]
            deal[unsafe { core::mem::transmute((index & 0x3) as u8) }].insert(*card);
        }

        deal
    }

    /// Shuffle the deck
    pub fn shuffle(&mut self) {
        self.cards.shuffle(&mut rand::thread_rng());
    }
}

/// Create a shuffled standard 52-card deck
#[must_use]
pub fn shuffled_standard_52_deck() -> Deck {
    let mut deck = Deck::standard_52();
    deck.shuffle();
    deck
}
