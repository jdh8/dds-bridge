#[cfg(test)]
mod test;

use crate::Strain;
use core::fmt;
use core::num::{NonZeroU8, Wrapping};
use core::ops::{Add, AddAssign, BitAnd, BitOr, BitXor, Index, IndexMut, Not, Sub, SubAssign};
use rand::prelude::SliceRandom as _;
use thiserror::Error;

/// A suit of playing cards
///
/// Suits are convertible to [`Strain`]s since suits form a subset of strains.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Suit {
    /// ♣, convertible to [`Strain::Clubs`]
    Clubs,
    /// ♦, convertible to [`Strain::Diamonds`]
    Diamonds,
    /// ♥, convertible to [`Strain::Hearts`]
    Hearts,
    /// ♠, convertible to [`Strain::Spades`]
    Spades,
}

impl Suit {
    /// Suits in the ascending order, the order in this crate
    pub const ASC: [Self; 4] = [Self::Clubs, Self::Diamonds, Self::Hearts, Self::Spades];

    /// Suits in the descending order, the order in [`dds_bridge_sys`]
    pub const DESC: [Self; 4] = [Self::Spades, Self::Hearts, Self::Diamonds, Self::Clubs];
}

impl From<Suit> for Strain {
    fn from(suit: Suit) -> Self {
        match suit {
            Suit::Clubs => Self::Clubs,
            Suit::Diamonds => Self::Diamonds,
            Suit::Hearts => Self::Hearts,
            Suit::Spades => Self::Spades,
        }
    }
}

/// Error raised when converting [`Strain::Notrump`] to a suit
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("Notrump is not a suit")]
pub struct SuitFromNotrumpError;

impl TryFrom<Strain> for Suit {
    type Error = SuitFromNotrumpError;

    fn try_from(strain: Strain) -> Result<Self, Self::Error> {
        match strain {
            Strain::Clubs => Ok(Self::Clubs),
            Strain::Diamonds => Ok(Self::Diamonds),
            Strain::Hearts => Ok(Self::Hearts),
            Strain::Spades => Ok(Self::Spades),
            Strain::Notrump => Err(SuitFromNotrumpError),
        }
    }
}

/// Position at the table
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl Seat {
    /// Seats in the order of dealing
    pub const ALL: [Self; 4] = [Self::North, Self::East, Self::South, Self::West];
}

impl Add<Wrapping<u8>> for Seat {
    type Output = Self;

    fn add(self, rhs: Wrapping<u8>) -> Self {
        // SAFETY: this is just modular arithmetics on a 4-element enum
        unsafe { core::mem::transmute((Wrapping(self as u8) + rhs).0 & 3) }
    }
}

impl Add<Seat> for Wrapping<u8> {
    type Output = Seat;

    fn add(self, rhs: Seat) -> Seat {
        rhs + self
    }
}

impl AddAssign<Wrapping<u8>> for Seat {
    fn add_assign(&mut self, rhs: Wrapping<u8>) {
        *self = *self + rhs;
    }
}

impl Sub<Wrapping<u8>> for Seat {
    type Output = Self;

    fn sub(self, rhs: Wrapping<u8>) -> Self {
        // SAFETY: this is just modular arithmetics on a 4-element enum
        unsafe { core::mem::transmute((Wrapping(self as u8) - rhs).0 & 3) }
    }
}

impl SubAssign<Wrapping<u8>> for Seat {
    fn sub_assign(&mut self, rhs: Wrapping<u8>) {
        *self = *self - rhs;
    }
}

impl Sub<Self> for Seat {
    type Output = Wrapping<u8>;

    fn sub(self, rhs: Self) -> Wrapping<u8> {
        (Wrapping(self as u8) - Wrapping(rhs as u8)) & Wrapping(3)
    }
}

impl From<Seat> for char {
    fn from(seat: Seat) -> Self {
        match seat {
            Seat::North => 'N',
            Seat::East => 'E',
            Seat::South => 'S',
            Seat::West => 'W',
        }
    }
}

bitflags::bitflags! {
    /// A set of seats
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct SeatFlags: u8 {
        /// The set containing [`Seat::North`]
        const NORTH = 0b0001;
        /// The set containing [`Seat::East`]
        const EAST = 0b0010;
        /// The set containing [`Seat::South`]
        const SOUTH = 0b0100;
        /// The set containing [`Seat::West`]
        const WEST = 0b1000;
    }
}

impl SeatFlags {
    /// The empty set
    pub const EMPTY: Self = Self::empty();

    /// The set containing all seats
    pub const ALL: Self = Self::all();

    /// The set containing [`Seat::North`] and [`Seat::South`]
    pub const NS: Self = Self::NORTH.union(Self::SOUTH);

    /// The set containing [`Seat::East`] and [`Seat::West`]
    pub const EW: Self = Self::EAST.union(Self::WEST);
}

impl From<Seat> for SeatFlags {
    fn from(seat: Seat) -> Self {
        match seat {
            Seat::North => Self::NORTH,
            Seat::East => Self::EAST,
            Seat::South => Self::SOUTH,
            Seat::West => Self::WEST,
        }
    }
}

/// A playing card
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Card(NonZeroU8);

impl Card {
    /// Create a card from suit and rank
    ///
    /// The rank is a number from 2 to 14.  J, Q, K, A are encoded as 11, 12,
    /// 13, 14 respectively.
    ///
    /// # Panics
    /// Panics if the rank is not in the range 2..=14.
    #[must_use]
    pub const fn new(suit: Suit, rank: u8) -> Self {
        assert!(rank >= 2 && rank <= 14);
        // SAFETY: rank is guaranteed to be non-zero
        Self(unsafe { NonZeroU8::new_unchecked(rank << 2 | suit as u8) })
    }

    /// The suit of the card
    #[must_use]
    pub const fn suit(self) -> Suit {
        // SAFETY: suit is guaranteed to be valid, in (0..=3)
        unsafe { core::mem::transmute(self.0.get() & 3) }
    }

    /// The rank of the card
    ///
    /// The rank is a number from 2 to 14.  J, Q, K, A are denoted as 11, 12,
    /// 13, 14 respectively.
    #[must_use]
    pub const fn rank(self) -> u8 {
        self.0.get() >> 2
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

    /// Iterate over the values in the set
    fn iter(self) -> impl Iterator<Item = T>;
}

/// A set of cards of the same suit
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Holding(u16);

#[derive(Debug, Clone, PartialEq, Eq)]
struct HoldingIter {
    rest: u16,
    cursor: u8,
}

impl Iterator for HoldingIter {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.rest == 0 {
            return None;
        }
        // 1. Trailing zeros are in the range of 0..=15, which fits in `u8``
        // 2. Trailing zeros cannot be 15 since the bitset is from a `Holding`
        #[allow(clippy::cast_possible_truncation)]
        let step = self.rest.trailing_zeros() as u8 + 1;
        self.rest >>= step;
        self.cursor += step;
        Some(self.cursor - 1)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.rest.count_ones() as usize;
        (count, Some(count))
    }

    fn count(self) -> usize {
        self.rest.count_ones() as usize
    }
}

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

    fn iter(self) -> impl Iterator<Item = u8> {
        HoldingIter {
            rest: self.0,
            cursor: 0,
        }
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

/// Show cards in descending order
///
/// 1. The ten is shown as `T` for PBN compatibility.
/// 2. This implementation ignores formatting flags for simplicity and speed.
///    If you want to pad or align the output, use [`fmt::Formatter::pad`].
impl fmt::Display for Holding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for rank in (2..15).rev() {
            if self.contains(rank) {
                use fmt::Write as _;
                f.write_char(b"23456789TJQKA"[rank as usize - 2] as char)?;
            }
        }
        Ok(())
    }
}

/// A hand of playing cards
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Hand(pub [Holding; 4]);

impl Index<Suit> for Hand {
    type Output = Holding;

    fn index(&self, suit: Suit) -> &Holding {
        &self.0[suit as usize]
    }
}

impl IndexMut<Suit> for Hand {
    fn index_mut(&mut self, suit: Suit) -> &mut Holding {
        &mut self.0[suit as usize]
    }
}

impl Hand {
    /// As a bitset of cards
    #[must_use]
    pub const fn to_bits(self) -> u64 {
        // SAFETY: every combination of 64 bits is a valid `u64`
        unsafe { core::mem::transmute(self.0) }
    }

    /// Create a hand from a bitset of cards
    ///
    /// This function removes invalid cards.
    #[must_use]
    pub const fn from_bits(bits: u64) -> Self {
        // SAFETY: just filtered out invalid cards
        unsafe { Self::from_bits_unchecked(bits & Self::ALL.to_bits()) }
    }

    /// Create a hand from a bitset of cards without checking
    ///
    /// # Safety
    /// The bitset must not contain invalid cards.
    #[must_use]
    pub const unsafe fn from_bits_unchecked(bits: u64) -> Self {
        core::mem::transmute(bits)
    }
}

impl SmallSet<Card> for Hand {
    const EMPTY: Self = Self([Holding::EMPTY; 4]);
    const ALL: Self = Self([Holding::ALL; 4]);

    fn len(self) -> usize {
        self.to_bits().count_ones() as usize
    }

    fn contains(self, card: Card) -> bool {
        self[card.suit()].contains(card.rank())
    }

    fn insert(&mut self, card: Card) -> bool {
        self[card.suit()].insert(card.rank())
    }

    fn remove(&mut self, card: Card) -> bool {
        self[card.suit()].remove(card.rank())
    }

    fn toggle(&mut self, card: Card) -> bool {
        self[card.suit()].toggle(card.rank())
    }

    fn iter(self) -> impl Iterator<Item = Card> {
        Suit::ASC
            .into_iter()
            .flat_map(move |suit| self[suit].iter().map(move |rank| Card::new(suit, rank)))
    }
}

/// PBN-compatible display of a hand
///
/// This implementation ignores formatting flags for simplicity and speed.
impl fmt::Display for Hand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}.{}.{}.{}",
            self[Suit::Spades],
            self[Suit::Hearts],
            self[Suit::Diamonds],
            self[Suit::Clubs]
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
pub struct Deal(pub [Hand; 4]);

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

struct DealDisplay {
    deal: Deal,
    seat: Seat,
}

impl fmt::Display for DealDisplay {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}:{} {} {} {}",
            char::from(self.seat),
            self.deal[self.seat],
            self.deal[self.seat + Wrapping(1)],
            self.deal[self.seat + Wrapping(2)],
            self.deal[self.seat + Wrapping(3)],
        )
    }
}

impl Deal {
    /// Create a deal from a shuffled standard 52-card deck
    pub fn new(rng: &mut (impl rand::Rng + ?Sized)) -> Self {
        let mut deck: [_; 52] = core::array::from_fn(|i| {
            // Safe because `i` is always in the range of 0..52
            #[allow(clippy::cast_possible_truncation)]
            let i = i as u8;
            let suit: Suit = unsafe { core::mem::transmute(i & 3) };
            Card::new(suit, (i >> 2) + 2)
        });
        deck.shuffle(rng);

        deck.into_iter()
            .enumerate()
            .fold(Self::default(), |mut deal, (i, card)| {
                // SAFETY: `i & 3` is always in the range of 0..=3
                #[allow(clippy::cast_possible_truncation)]
                let seat: Seat = unsafe { core::mem::transmute((i & 3) as u8) };
                deal[seat].insert(card);
                deal
            })
    }

    /// PBN-compatible display from a seat's perspective
    #[must_use]
    pub fn display(self, seat: Seat) -> impl fmt::Display {
        DealDisplay { deal: self, seat }
    }

    /// Shuffle existing hands while preserving the numbers of cards
    #[must_use]
    pub fn shuffled(self, rng: &mut (impl rand::Rng + ?Sized), seats: SeatFlags) -> Self {
        let mut deal = Self::default();
        let mut deck = Vec::with_capacity(52);
        let mut lengths = [0; 4];

        for seat in Seat::ALL {
            if seats.contains(seat.into()) {
                deck.extend(self[seat].iter());
                lengths[seat as usize] = self[seat].len();
            } else {
                deal[seat] = self[seat];
            }
        }

        deck.shuffle(rng);
        let mut seat = Seat::North;

        loop {
            let Some(card) = deck.pop() else { break };

            while lengths[seat as usize] == 0 {
                seat += Wrapping(1);
            }

            lengths[seat as usize] -= 1;
            deal[seat].insert(card);
        }
        deal
    }
}
