use crate::Suit;
use core::fmt::{self, Write as _};
use core::iter::FusedIterator;
use core::num::NonZero;
use core::ops;
use core::str::FromStr;
use thiserror::Error;

/// Position at the table
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

    /// The partner of the seat
    #[must_use]
    pub const fn partner(self) -> Self {
        match self {
            Self::North => Self::South,
            Self::East => Self::West,
            Self::South => Self::North,
            Self::West => Self::East,
        }
    }

    /// The opponent on the left of the seat
    #[must_use]
    pub const fn lho(self) -> Self {
        match self {
            Self::North => Self::East,
            Self::East => Self::South,
            Self::South => Self::West,
            Self::West => Self::North,
        }
    }

    /// The opponent on the right of the seat
    #[must_use]
    pub const fn rho(self) -> Self {
        match self {
            Self::North => Self::West,
            Self::East => Self::North,
            Self::South => Self::East,
            Self::West => Self::South,
        }
    }

    /// Display character for this seat
    #[must_use]
    #[inline]
    pub const fn letter(self) -> char {
        match self {
            Self::North => 'N',
            Self::East => 'E',
            Self::South => 'S',
            Self::West => 'W',
        }
    }
}

const _: () = assert!(Seat::ALL[0] as u8 == 0);
const _: () = assert!(Seat::ALL[1] as u8 == 1);
const _: () = assert!(Seat::ALL[2] as u8 == 2);
const _: () = assert!(Seat::ALL[3] as u8 == 3);

impl fmt::Display for Seat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(self.letter())
    }
}

/// Error returned when parsing a [`Seat`] fails
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("Invalid seat: expected one of N, E, S, W (or full names)")]
pub struct ParseSeatError;

impl FromStr for Seat {
    type Err = ParseSeatError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_uppercase().as_str() {
            "N" | "NORTH" => Ok(Self::North),
            "E" | "EAST" => Ok(Self::East),
            "S" | "SOUTH" => Ok(Self::South),
            "W" | "WEST" => Ok(Self::West),
            _ => Err(ParseSeatError),
        }
    }
}

bitflags::bitflags! {
    /// A set of seats
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

/// Error indicating an invalid rank
///
/// The rank of a card must be in `2..=14`, where J, Q, K, A are denoted as 11,
/// 12, 13, 14 respectively.
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[error("{0} is not a valid rank (2..=14)")]
pub struct InvalidRank(u8);

/// The rank of a card, from 2 to 14, where J, Q, K, A are internally denoted as
/// 11, 12, 13, 14 respectively.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Rank(NonZero<u8>);

impl Rank {
    /// Ace
    pub const A: Self = Self(NonZero::new(14).unwrap());

    /// King
    pub const K: Self = Self(NonZero::new(13).unwrap());

    /// Queen
    pub const Q: Self = Self(NonZero::new(12).unwrap());

    /// Jack
    pub const J: Self = Self(NonZero::new(11).unwrap());

    /// Ten
    pub const T: Self = Self(NonZero::new(10).unwrap());

    /// Create a rank from a number
    ///
    /// # Panics
    ///
    /// When the rank is not in `2..=14`.  In const contexts, this is a
    /// compile-time error.
    #[must_use]
    #[inline]
    pub const fn new(rank: u8) -> Self {
        match Self::try_new(rank) {
            Ok(r) => r,
            Err(_) => panic!("rank must be in 2..=14"),
        }
    }

    /// Try to create a rank from a number
    ///
    /// # Errors
    ///
    /// When the rank is not in `2..=14`.
    #[inline]
    pub const fn try_new(rank: u8) -> Result<Self, InvalidRank> {
        match NonZero::new(rank) {
            Some(nonzero) if rank >= 2 && rank <= 14 => Ok(Self(nonzero)),
            _ => Err(InvalidRank(rank)),
        }
    }

    /// Get the stored rank as [`u8`]
    #[must_use]
    #[inline]
    pub const fn get(self) -> u8 {
        self.0.get()
    }

    /// Display character for this rank
    #[must_use]
    #[inline]
    pub const fn letter(self) -> char {
        b"23456789TJQKA"[self.get() as usize - 2] as char
    }
}

impl fmt::Display for Rank {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(self.letter())
    }
}

/// Error returned when parsing a [`Rank`] fails
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("Invalid rank: expected 2-10, T, J, Q, K, A")]
pub struct ParseRankError;

impl FromStr for Rank {
    type Err = ParseRankError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_uppercase().as_str() {
            "A" => Ok(Self::A),
            "K" => Ok(Self::K),
            "Q" => Ok(Self::Q),
            "J" => Ok(Self::J),
            "T" | "10" => Ok(Self::T),
            "9" => Ok(Self::new(9)),
            "8" => Ok(Self::new(8)),
            "7" => Ok(Self::new(7)),
            "6" => Ok(Self::new(6)),
            "5" => Ok(Self::new(5)),
            "4" => Ok(Self::new(4)),
            "3" => Ok(Self::new(3)),
            "2" => Ok(Self::new(2)),
            _ => Err(ParseRankError),
        }
    }
}

/// A playing card
///
/// Internally packed as `(rank << 2) | suit` in a single byte.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Card {
    /// The suit of the card
    pub suit: Suit,
    /// The rank of the card
    pub rank: Rank,
}

impl fmt::Display for Card {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.suit, self.rank)
    }
}

/// Error returned when parsing a [`Card`] fails
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
pub enum ParseCardError {
    /// Invalid suit in card
    #[error("Invalid suit in card: expected <suit><rank>, e.g. S7, ♥A, ♢10")]
    Suit,
    /// Invalid rank in card
    #[error("Invalid rank in card: expected <suit><rank>, e.g. S7, ♥A, ♢10")]
    Rank,
}

impl FromStr for Card {
    type Err = ParseCardError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rank_len = if s.ends_with("10") {
            2
        } else {
            s.chars().next_back().map_or(0, char::len_utf8)
        };
        let border = s.len().saturating_sub(rank_len);
        let (suit, rank) = s.split_at(border);
        let suit: Suit = suit.parse().map_err(|_| ParseCardError::Suit)?;
        let rank: Rank = rank.parse().map_err(|_| ParseCardError::Rank)?;
        Ok(Self { suit, rank })
    }
}

/// A set of cards of the same suit
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Holding(u16);

/// Iterator over the ranks in a [`Holding`], yielding [`Rank`]s in descending order
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoldingIter {
    rest: u16,
    cursor: u8,
}

impl Iterator for HoldingIter {
    type Item = Rank;

    fn next(&mut self) -> Option<Self::Item> {
        if self.rest == 0 {
            return None;
        }

        // For non-zero u16, leading_zeros is in 0..=15, fitting in u8.
        // Bit 15 is never set in a valid Holding (max rank is 14), so pos <= 14.
        #[allow(clippy::cast_possible_truncation)]
        let pos = 15 - self.rest.leading_zeros() as u8;
        self.rest &= !(1u16 << pos);
        let rank = self.cursor + pos;

        // SAFETY: rank is in 2..=14 by construction from a valid Holding
        Some(Rank(unsafe { core::num::NonZero::new_unchecked(rank) }))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.rest.count_ones() as usize;
        (count, Some(count))
    }

    fn count(self) -> usize {
        self.rest.count_ones() as usize
    }
}

impl DoubleEndedIterator for HoldingIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.rest == 0 {
            return None;
        }

        // 1. Trailing zeros are in the range of 0..=15, which fits in `u8`
        // 2. Trailing zeros cannot be 15 since the bitset is from a `Holding`
        #[allow(clippy::cast_possible_truncation)]
        let step = self.rest.trailing_zeros() as u8 + 1;
        self.rest >>= step;
        self.cursor += step;

        // SAFETY: cursor is in 2..=14 by construction from a valid Holding
        Some(Rank(unsafe {
            core::num::NonZero::new_unchecked(self.cursor - 1)
        }))
    }
}

impl ExactSizeIterator for HoldingIter {
    fn len(&self) -> usize {
        self.rest.count_ones() as usize
    }
}

impl FusedIterator for HoldingIter {}

impl Holding {
    /// The empty set
    pub const EMPTY: Self = Self(0);

    /// The set containing all possible ranks (2..=14)
    pub const ALL: Self = Self(0x7FFC);

    /// The number of cards in the holding
    #[must_use]
    #[inline]
    pub const fn len(self) -> usize {
        self.0.count_ones() as usize
    }

    /// Whether the holding is empty
    #[must_use]
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// Whether the holding contains a rank
    #[must_use]
    #[inline]
    pub const fn contains(self, rank: Rank) -> bool {
        self.0 & 1 << rank.get() != 0
    }

    /// Insert a rank into the holding, returning whether it was newly inserted
    #[inline]
    pub const fn insert(&mut self, rank: Rank) -> bool {
        let insertion = 1 << rank.get() & Self::ALL.0;
        let inserted = insertion & !self.0 != 0;
        self.0 |= insertion;
        inserted
    }

    /// Remove a rank from the holding, returning whether it was present
    #[inline]
    pub const fn remove(&mut self, rank: Rank) -> bool {
        let removed = self.contains(rank);
        self.0 &= !(1 << rank.get());
        removed
    }

    /// Toggle a rank in the holding, returning whether it is now present
    #[inline]
    pub const fn toggle(&mut self, rank: Rank) -> bool {
        self.0 ^= 1 << rank.get() & Self::ALL.0;
        self.contains(rank)
    }

    /// Conditionally insert/remove a rank from the holding
    #[inline]
    pub fn set(&mut self, rank: Rank, condition: bool) {
        let flag = 1 << rank.get();
        let mask = u16::from(condition).wrapping_neg();
        self.0 = (self.0 & !flag) | (mask & flag);
    }

    /// Iterate over the ranks in the holding
    #[inline]
    #[must_use]
    pub const fn iter(self) -> HoldingIter {
        HoldingIter {
            rest: self.0,
            cursor: 0,
        }
    }

    /// As a bitset of ranks
    #[must_use]
    #[inline]
    pub const fn to_bits(self) -> u16 {
        self.0
    }

    /// Create a holding from a bitset of ranks, retaining invalid ranks
    #[must_use]
    #[inline]
    pub const fn from_bits_retain(bits: u16) -> Self {
        Self(bits)
    }

    /// Whether the holding contains an invalid rank
    #[must_use]
    #[inline]
    pub const fn contains_unknown_bits(self) -> bool {
        self.0 & Self::ALL.0 != self.0
    }

    /// Create a holding from a bitset of ranks, checking for invalid ranks
    #[must_use]
    #[inline]
    pub const fn from_bits(bits: u16) -> Option<Self> {
        if bits & Self::ALL.0 == bits {
            Some(Self(bits))
        } else {
            None
        }
    }

    /// Create a holding from a bitset of ranks, removing invalid ranks
    #[must_use]
    #[inline]
    pub const fn from_bits_truncate(bits: u16) -> Self {
        Self(bits & Self::ALL.0)
    }

    /// Create a holding from a rank
    #[must_use]
    #[inline]
    pub const fn from_rank(rank: Rank) -> Self {
        Self(1 << rank.get())
    }
}

impl IntoIterator for Holding {
    type Item = Rank;
    type IntoIter = HoldingIter;

    fn into_iter(self) -> HoldingIter {
        self.iter()
    }
}

impl ops::BitAnd for Holding {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }
}

impl ops::BitOr for Holding {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

impl ops::BitXor for Holding {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self {
        Self(self.0 ^ rhs.0)
    }
}

impl ops::Not for Holding {
    type Output = Self;

    fn not(self) -> Self {
        Self::from_bits_truncate(!self.0)
    }
}

impl ops::Sub for Holding {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Self(self.0 & !rhs.0)
    }
}

impl ops::BitAndAssign for Holding {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl ops::BitOrAssign for Holding {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl ops::BitXorAssign for Holding {
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs;
    }
}

impl ops::SubAssign for Holding {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

/// Show cards in descending order
///
/// 1. The ten is shown as `T` for PBN compatibility.
/// 2. This implementation ignores formatting flags for simplicity and speed.
///    If you want to pad or align the output, use [`fmt::Formatter::pad`].
impl fmt::Display for Holding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for rank in (2u8..15).rev() {
            if self.0 & 1 << rank != 0 {
                f.write_char(b"23456789TJQKA"[rank as usize - 2] as char)?;
            }
        }
        Ok(())
    }
}

/// An error which can be returned when parsing a [`Holding`]
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParseHoldingError {
    /// Ranks are not all valid or in descending order
    #[error("Ranks are not all valid or in descending order")]
    InvalidRanks,

    /// The same rank appears more than once
    #[error("The same rank appears more than once")]
    RepeatedRank,

    /// A suit contains more than 13 cards
    #[error("A suit contains more than 13 cards")]
    TooManyCards,
}

/// An error which can be returned when parsing a [`Hand`]
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParseHandError {
    /// Error in a holding
    #[error(transparent)]
    Holding(#[from] ParseHoldingError),

    /// The hand does not contain 4 suits
    #[error("The hand does not contain 4 suits")]
    NotFourSuits,
}

/// An error which can be returned when parsing a [`Deal`]
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParseDealError {
    /// Invalid dealer tag
    #[error("Invalid dealer tag for a deal")]
    InvalidDealer,

    /// Error in a hand
    #[error(transparent)]
    Hand(#[from] ParseHandError),

    /// The deal does not contain 4 hands
    #[error("The deal does not contain 4 hands")]
    NotFourHands,
}

impl FromStr for Holding {
    type Err = ParseHoldingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // 13 cards + 1 extra char for "10"
        if s.len() > 14 {
            return Err(ParseHoldingError::TooManyCards);
        }

        let bytes = s.as_bytes();
        let mut i = 0;
        let mut prev_rank: u8 = 15;
        let mut explicit = Self::EMPTY;

        while i < bytes.len() {
            let c = bytes[i].to_ascii_uppercase();
            let rank: u8 = match c {
                b'A' => 14,
                b'K' => 13,
                b'Q' => 12,
                b'J' => 11,
                b'T' => 10,
                b'1' => {
                    if bytes.get(i + 1) != Some(&b'0') {
                        return Err(ParseHoldingError::InvalidRanks);
                    }
                    i += 1;
                    10
                }
                b'2'..=b'9' => c - b'0',
                b'X' => break,
                _ => return Err(ParseHoldingError::InvalidRanks),
            };

            if rank >= prev_rank {
                return Err(ParseHoldingError::InvalidRanks);
            }
            prev_rank = rank;

            // SAFETY: rank is in 2..=14 by construction above
            let r = Rank(unsafe { core::num::NonZero::new_unchecked(rank) });
            explicit.insert(r);
            i += 1;
        }

        let spot_count = bytes.len() - i;
        if bytes[i..].iter().any(|&b| !b.eq_ignore_ascii_case(&b'x')) {
            return Err(ParseHoldingError::InvalidRanks);
        }
        if spot_count > 13 {
            return Err(ParseHoldingError::TooManyCards);
        }

        let spots = Self::from_bits_truncate((4u16 << spot_count) - 4);

        if explicit & spots == Self::EMPTY {
            Ok(explicit | spots)
        } else {
            Err(ParseHoldingError::RepeatedRank)
        }
    }
}

/// A hand of playing cards
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Hand([Holding; 4]);

/// Iterator over the cards in a [`Hand`], yielding [`Card`]s in descending
/// suit order and descending rank order within each suit
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandIter {
    suits: [HoldingIter; 4],
    fwd: u8,
    bwd: u8,
}

impl Iterator for HandIter {
    type Item = Card;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.fwd > 3 {
                return None;
            }
            let suit = Suit::ASC[self.fwd as usize];
            if let Some(rank) = self.suits[self.fwd as usize].next() {
                return Some(Card { suit, rank });
            }
            if self.fwd == self.bwd {
                self.fwd = 4;
                return None;
            }
            self.fwd -= 1;
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.len();
        (count, Some(count))
    }

    fn count(self) -> usize {
        self.len()
    }
}

impl DoubleEndedIterator for HandIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        loop {
            if self.fwd > 3 {
                return None;
            }
            let suit = Suit::ASC[self.bwd as usize];
            if let Some(rank) = self.suits[self.bwd as usize].next_back() {
                return Some(Card { suit, rank });
            }
            if self.fwd == self.bwd {
                self.fwd = 4;
                return None;
            }
            self.bwd += 1;
        }
    }
}

impl ExactSizeIterator for HandIter {
    fn len(&self) -> usize {
        if self.fwd > 3 {
            return 0;
        }
        (self.bwd as usize..=self.fwd as usize)
            .map(|i| self.suits[i].len())
            .sum()
    }
}

impl FusedIterator for HandIter {}

impl ops::Index<Suit> for Hand {
    type Output = Holding;

    #[inline]
    fn index(&self, suit: Suit) -> &Holding {
        &self.0[suit as usize]
    }
}

impl ops::IndexMut<Suit> for Hand {
    #[inline]
    fn index_mut(&mut self, suit: Suit) -> &mut Holding {
        &mut self.0[suit as usize]
    }
}

impl Hand {
    /// As a bitset of cards
    #[must_use]
    #[inline]
    pub const fn to_bits(self) -> u64 {
        unsafe { core::mem::transmute(self.0) }
    }

    /// Create a hand from a bitset of cards, retaining invalid cards
    #[must_use]
    #[inline]
    pub const fn from_bits_retain(bits: u64) -> Self {
        unsafe { core::mem::transmute(bits) }
    }

    /// Whether the hand contains an invalid card
    #[must_use]
    #[inline]
    pub const fn contains_unknown_bits(self) -> bool {
        self.to_bits() & Self::ALL.to_bits() != self.to_bits()
    }

    /// Create a hand from a bitset of cards, checking for invalid cards
    #[must_use]
    #[inline]
    pub const fn from_bits(bits: u64) -> Option<Self> {
        if bits & Self::ALL.to_bits() == bits {
            Some(Self::from_bits_retain(bits))
        } else {
            None
        }
    }

    /// Create a hand from a bitset of cards, removing invalid cards
    #[must_use]
    #[inline]
    pub const fn from_bits_truncate(bits: u64) -> Self {
        Self::from_bits_retain(bits & Self::ALL.to_bits())
    }

    /// Create a hand from four holdings in suit order (clubs, diamonds, hearts, spades)
    #[must_use]
    #[inline]
    pub const fn new(clubs: Holding, diamonds: Holding, hearts: Holding, spades: Holding) -> Self {
        Self([clubs, diamonds, hearts, spades])
    }

    /// The empty hand
    pub const EMPTY: Self = Self([Holding::EMPTY; 4]);

    /// The hand containing all 52 cards
    pub const ALL: Self = Self([Holding::ALL; 4]);

    /// The number of cards in the hand
    #[must_use]
    #[inline]
    pub const fn len(self) -> usize {
        self.to_bits().count_ones() as usize
    }

    /// Whether the hand is empty
    #[must_use]
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.to_bits() == 0
    }

    /// Whether the hand contains a card
    #[must_use]
    #[inline]
    pub fn contains(self, card: Card) -> bool {
        self[card.suit].contains(card.rank)
    }

    /// Insert a card into the hand, returning whether it was newly inserted
    #[inline]
    pub fn insert(&mut self, card: Card) -> bool {
        self[card.suit].insert(card.rank)
    }

    /// Remove a card from the hand, returning whether it was present
    #[inline]
    pub fn remove(&mut self, card: Card) -> bool {
        self[card.suit].remove(card.rank)
    }

    /// Toggle a card in the hand, returning whether it is now present
    #[inline]
    pub fn toggle(&mut self, card: Card) -> bool {
        self[card.suit].toggle(card.rank)
    }

    /// Conditionally insert/remove a card from the hand
    #[inline]
    pub fn set(&mut self, card: Card, condition: bool) {
        self[card.suit].set(card.rank, condition);
    }

    /// Iterate over the cards in the hand
    #[inline]
    #[must_use]
    pub const fn iter(self) -> HandIter {
        HandIter {
            suits: [
                self.0[0].iter(),
                self.0[1].iter(),
                self.0[2].iter(),
                self.0[3].iter(),
            ],
            fwd: 3,
            bwd: 0,
        }
    }
}

impl IntoIterator for Hand {
    type Item = Card;
    type IntoIter = HandIter;

    #[inline]
    fn into_iter(self) -> HandIter {
        self.iter()
    }
}

impl FromIterator<Card> for Hand {
    fn from_iter<I: IntoIterator<Item = Card>>(iter: I) -> Self {
        iter.into_iter().fold(Self::EMPTY, |mut hand, card| {
            hand.insert(card);
            hand
        })
    }
}

/// PBN-compatible display of a hand
///
/// This implementation ignores formatting flags for simplicity and speed.
impl fmt::Display for Hand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_empty() {
            return f.write_char('-');
        }

        self[Suit::Spades].fmt(f)?;
        f.write_char('.')?;

        self[Suit::Hearts].fmt(f)?;
        f.write_char('.')?;

        self[Suit::Diamonds].fmt(f)?;
        f.write_char('.')?;

        self[Suit::Clubs].fmt(f)
    }
}

impl FromStr for Hand {
    type Err = ParseHandError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // 52 cards + 4 tens + 3 dots
        if s.len() > 52 + 4 + 3 {
            return Err(ParseHoldingError::TooManyCards.into());
        }

        if s == "-" {
            return Ok(Self::EMPTY);
        }

        let holdings: Result<Vec<_>, _> = s.split('.').map(Holding::from_str).rev().collect();

        Ok(Self(
            holdings?
                .try_into()
                .map_err(|_| ParseHandError::NotFourSuits)?,
        ))
    }
}

impl ops::BitAnd for Hand {
    type Output = Self;

    #[inline]
    fn bitand(self, rhs: Self) -> Self {
        Self::from_bits_retain(self.to_bits() & rhs.to_bits())
    }
}

impl ops::BitOr for Hand {
    type Output = Self;

    #[inline]
    fn bitor(self, rhs: Self) -> Self {
        Self::from_bits_retain(self.to_bits() | rhs.to_bits())
    }
}

impl ops::BitXor for Hand {
    type Output = Self;

    #[inline]
    fn bitxor(self, rhs: Self) -> Self {
        Self::from_bits_retain(self.to_bits() ^ rhs.to_bits())
    }
}

impl ops::Not for Hand {
    type Output = Self;

    #[inline]
    fn not(self) -> Self {
        Self::from_bits_truncate(!self.to_bits())
    }
}

impl ops::Sub for Hand {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self {
        Self::from_bits_retain(self.to_bits() & !rhs.to_bits())
    }
}

impl ops::BitAndAssign for Hand {
    #[inline]
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl ops::BitOrAssign for Hand {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl ops::BitXorAssign for Hand {
    #[inline]
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs;
    }
}

impl ops::SubAssign for Hand {
    #[inline]
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

/// A deal of four hands
///
/// Parses the [PBN] deal format: `<dealer>:<hand> <hand> <hand> <hand>`,
/// where each hand is four dot-separated holdings ordered spades, hearts,
/// diamonds, clubs.  Hands are listed clockwise starting from the dealer.
///
/// # Examples
///
/// ```
/// use dds_bridge::{Deal, Hand, Seat, Suit};
///
/// let deal: Deal = "N:AKQJT.AKQJT.AKQ.2 98765.98765.JT98.3 432.432.765432.4 -:T9876543.".parse().unwrap_or_default();
/// let _ = deal[Seat::North][Suit::Spades];
/// ```
///
/// [PBN]: https://www.tistis.nl/pbn/
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Deal([Hand; 4]);

impl IntoIterator for Deal {
    type Item = Hand;
    type IntoIter = core::array::IntoIter<Hand, 4>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl ops::Index<Seat> for Deal {
    type Output = Hand;

    #[inline]
    fn index(&self, seat: Seat) -> &Hand {
        &self.0[seat as usize]
    }
}

impl ops::IndexMut<Seat> for Deal {
    #[inline]
    fn index_mut(&mut self, seat: Seat) -> &mut Hand {
        &mut self.0[seat as usize]
    }
}

impl Deal {
    /// Empty deal
    pub const EMPTY: Self = Self([Hand::EMPTY; 4]);

    /// Construct a deal from four hands
    #[must_use]
    pub const fn new(north: Hand, east: Hand, south: Hand, west: Hand) -> Self {
        Self([north, east, south, west])
    }

    /// If the deal is a subset of a bridge deal, collect all the cards into a
    /// single hand.  Otherwise, return `None`.  This function checks the
    /// validity of the deal and also returns potentially useful information.
    ///
    /// A deal is a subset of a bridge deal if it satisfies the following
    /// conditions:
    ///
    /// 1. Each hand contains at most 13 cards.
    /// 2. The hands are pairwise disjoint.
    #[must_use]
    pub fn validate_and_collect(self) -> Option<Hand> {
        let mut seen = Hand::EMPTY;
        for hand in self.0 {
            if hand.len() > 13 || hand & seen != Hand::EMPTY {
                return None;
            }
            seen |= hand;
        }
        Some(seen)
    }

    /// PBN-compatible display from a seat's perspective
    #[must_use]
    pub fn display(self, seat: Seat) -> impl fmt::Display {
        struct DisplayAt {
            deal: Deal,
            seat: Seat,
        }
        impl fmt::Display for DisplayAt {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_char(self.seat.letter())?;
                f.write_char(':')?;

                self.deal[self.seat].fmt(f)?;
                f.write_char(' ')?;

                self.deal[self.seat.lho()].fmt(f)?;
                f.write_char(' ')?;

                self.deal[self.seat.partner()].fmt(f)?;
                f.write_char(' ')?;

                self.deal[self.seat.rho()].fmt(f)
            }
        }
        DisplayAt { deal: self, seat }
    }
}

impl FromStr for Deal {
    type Err = ParseDealError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let bytes = s.as_bytes();

        let dealer = match bytes.first().map(u8::to_ascii_uppercase) {
            Some(b'N') => Seat::North,
            Some(b'E') => Seat::East,
            Some(b'S') => Seat::South,
            Some(b'W') => Seat::West,
            _ => return Err(ParseDealError::InvalidDealer),
        };

        if bytes.get(1) != Some(&b':') {
            return Err(ParseDealError::InvalidDealer);
        }

        let hands: Result<Vec<_>, _> = s[2..].split_whitespace().map(Hand::from_str).collect();

        let mut deal = Self(
            hands?
                .try_into()
                .map_err(|_| ParseDealError::NotFourHands)?,
        );
        deal.0.rotate_right(dealer as usize);
        Ok(deal)
    }
}

#[cfg(feature = "serde")]
mod serde_string {
    use super::{Card, Deal, Hand, Holding, Seat};
    use core::fmt::Display;
    use core::str::FromStr;
    use serde::{Deserialize, Deserializer, Serialize, Serializer, de};

    fn serialize<T: Display, S: Serializer>(value: &T, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(value)
    }

    fn deserialize<'de, T, D>(deserializer: D) -> Result<T, D::Error>
    where
        T: FromStr,
        T::Err: Display,
        D: Deserializer<'de>,
    {
        let s = <&str>::deserialize(deserializer)?;
        s.parse().map_err(de::Error::custom)
    }

    impl Serialize for Card {
        fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
            serialize(self, s)
        }
    }
    impl<'de> Deserialize<'de> for Card {
        fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
            deserialize(d)
        }
    }

    impl Serialize for Holding {
        fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
            serialize(self, s)
        }
    }
    impl<'de> Deserialize<'de> for Holding {
        fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
            deserialize(d)
        }
    }

    impl Serialize for Hand {
        fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
            serialize(self, s)
        }
    }
    impl<'de> Deserialize<'de> for Hand {
        fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
            deserialize(d)
        }
    }

    impl Serialize for Deal {
        fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
            s.collect_str(&self.display(Seat::North))
        }
    }
    impl<'de> Deserialize<'de> for Deal {
        fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
            deserialize(d)
        }
    }
}
