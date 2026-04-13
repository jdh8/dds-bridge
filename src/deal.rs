use crate::Suit;
use core::fmt::{self, Write as _};
use core::num::NonZero;
use core::ops;
use core::str::FromStr;
use std::sync::LazyLock;
use thiserror::Error;

/// Position at the table
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
}

const _: () = assert!(Seat::ALL[0] as u8 == 0);
const _: () = assert!(Seat::ALL[1] as u8 == 1);
const _: () = assert!(Seat::ALL[2] as u8 == 2);
const _: () = assert!(Seat::ALL[3] as u8 == 3);

impl From<Seat> for char {
    #[inline]
    fn from(seat: Seat) -> Self {
        match seat {
            Seat::North => 'N',
            Seat::East => 'E',
            Seat::South => 'S',
            Seat::West => 'W',
        }
    }
}

impl fmt::Display for Seat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(char::from(*self))
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
    #[inline]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

/// A playing card
///
/// Internally packed as `(rank << 2) | suit` in a single byte.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Card(NonZero<u8>);

impl Card {
    /// Create a card from suit and rank
    #[must_use]
    #[inline]
    pub const fn new(suit: Suit, rank: Rank) -> Self {
        // SAFETY: rank is in 2..=14, so (rank << 2 | suit) is always nonzero
        Self(unsafe { NonZero::new_unchecked(rank.get() << 2 | suit as u8) })
    }

    /// The suit of the card
    #[must_use]
    #[inline]
    pub const fn suit(self) -> Suit {
        // SAFETY: the low 2 bits are always a valid Suit (0..=3) by construction
        unsafe { core::mem::transmute(self.0.get() & 3) }
    }

    /// The rank of the card
    #[must_use]
    #[inline]
    pub const fn rank(self) -> Rank {
        // SAFETY: the stored rank is always in 2..=14 by construction
        unsafe { Rank(core::num::NonZero::new_unchecked(self.0.get() >> 2)) }
    }
}

impl fmt::Display for Card {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.suit(), self.rank())
    }
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
    type Item = Rank;

    #[inline]
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
        // SAFETY: cursor is in 2..=14 by construction from a valid Holding
        Some(Rank(unsafe {
            core::num::NonZero::new_unchecked(self.cursor - 1)
        }))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.rest.count_ones() as usize;
        (count, Some(count))
    }

    #[inline]
    fn count(self) -> usize {
        self.rest.count_ones() as usize
    }
}

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
    pub fn insert(&mut self, rank: Rank) -> bool {
        let insertion = 1 << rank.get() & Self::ALL.0;
        let inserted = insertion & !self.0 != 0;
        self.0 |= insertion;
        inserted
    }

    /// Remove a rank from the holding, returning whether it was present
    #[inline]
    pub fn remove(&mut self, rank: Rank) -> bool {
        let removed = self.contains(rank);
        self.0 &= !(1 << rank.get());
        removed
    }

    /// Toggle a rank in the holding, returning whether it is now present
    #[inline]
    pub fn toggle(&mut self, rank: Rank) -> bool {
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
    pub fn iter(self) -> impl Iterator<Item = Rank> {
        HoldingIter {
            rest: self.0,
            cursor: 0,
        }
    }
}

impl Holding {
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

impl ops::BitAnd for Holding {
    type Output = Self;

    #[inline]
    fn bitand(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }
}

impl ops::BitOr for Holding {
    type Output = Self;

    #[inline]
    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

impl ops::BitXor for Holding {
    type Output = Self;

    #[inline]
    fn bitxor(self, rhs: Self) -> Self {
        Self(self.0 ^ rhs.0)
    }
}

impl ops::Not for Holding {
    type Output = Self;

    #[inline]
    fn not(self) -> Self {
        Self::from_bits_truncate(!self.0)
    }
}

impl ops::Sub for Holding {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self {
        Self(self.0 & !rhs.0)
    }
}

impl ops::BitAndAssign for Holding {
    #[inline]
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl ops::BitOrAssign for Holding {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl ops::BitXorAssign for Holding {
    #[inline]
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs;
    }
}

impl ops::SubAssign for Holding {
    #[inline]
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

/// An error which can be returned when parsing a [`Holding`], a [`Hand`], or a [`Deal`]
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParseHandError {
    /// Ranks are not all valid or in descending order
    #[error("Ranks are not all valid or in descending order")]
    InvalidHolding,

    /// The same rank appears more than once
    #[error("The same rank appears more than once")]
    RepeatedRank,

    /// A suit contains more than 13 cards
    #[error("A suit contains more than 13 cards")]
    TooManyCards,

    /// The hand does not contain 4 suits
    #[error("The hand does not contain 4 suits")]
    NotFourSuits,

    /// Invalid dealer tag for a deal
    #[error("Invalid dealer tag for a deal")]
    InvalidDealer,

    /// The deal does not contain 4 hands
    #[error("The deal does not contain 4 hands")]
    NotFourHands,
}

impl FromStr for Holding {
    type Err = ParseHandError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        static RE: LazyLock<regex::Regex> = LazyLock::new(|| {
            regex::RegexBuilder::new("^(A?K?Q?J?(?:T|10)?9?8?7?6?5?4?3?2?)(x*)$")
                .case_insensitive(true)
                .build()
                .unwrap()
        });

        // 13 cards + 1 ten
        if s.len() > 13 + 1 {
            return Err(ParseHandError::TooManyCards);
        }

        let Some((_, [explicit, spots])) = RE.captures(s).map(|x| x.extract()) else {
            return Err(ParseHandError::InvalidHolding);
        };

        let explicit = explicit.bytes().try_fold(Self::EMPTY, |mut holding, c| {
            let rank = match c.to_ascii_uppercase() {
                b'1' => return Ok(holding),
                b'2' => 2,
                b'3' => 3,
                b'4' => 4,
                b'5' => 5,
                b'6' => 6,
                b'7' => 7,
                b'8' => 8,
                b'9' => 9,
                b'T' | b'0' => 10,
                b'J' => 11,
                b'Q' => 12,
                b'K' => 13,
                b'A' => 14,
                _ => unreachable!("Invalid ranks should have been caught by the regex"),
            };

            // SAFETY: rank is in 2..=14 by construction above
            let rank = Rank(unsafe { core::num::NonZero::new_unchecked(rank) });
            if holding.insert(rank) {
                Ok(holding)
            } else {
                Err(ParseHandError::RepeatedRank)
            }
        })?;

        let spots = Self::from_bits_truncate((4 << spots.len()) - 4);

        if explicit & spots == Self::EMPTY {
            Ok(explicit | spots)
        } else {
            Err(ParseHandError::RepeatedRank)
        }
    }
}

/// A hand of playing cards
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Hand([Holding; 4]);

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
}

impl Hand {
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
        self[card.suit()].contains(card.rank())
    }

    /// Insert a card into the hand, returning whether it was newly inserted
    #[inline]
    pub fn insert(&mut self, card: Card) -> bool {
        self[card.suit()].insert(card.rank())
    }

    /// Remove a card from the hand, returning whether it was present
    #[inline]
    pub fn remove(&mut self, card: Card) -> bool {
        self[card.suit()].remove(card.rank())
    }

    /// Toggle a card in the hand, returning whether it is now present
    #[inline]
    pub fn toggle(&mut self, card: Card) -> bool {
        self[card.suit()].toggle(card.rank())
    }

    /// Conditionally insert/remove a card from the hand
    #[inline]
    pub fn set(&mut self, card: Card, condition: bool) {
        self[card.suit()].set(card.rank(), condition);
    }

    /// Iterate over the cards in the hand
    #[inline]
    pub fn iter(self) -> impl Iterator<Item = Card> {
        Suit::ASC
            .into_iter()
            .flat_map(move |suit| self[suit].iter().map(move |rank| Card::new(suit, rank)))
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
            return Err(ParseHandError::TooManyCards);
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
                f.write_char(char::from(self.seat))?;
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
    type Err = ParseHandError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        static DEALER: LazyLock<regex::Regex> = LazyLock::new(|| {
            regex::RegexBuilder::new(r"^[NESW]:\s*")
                .case_insensitive(true)
                .build()
                .unwrap()
        });

        let Some(tag) = DEALER.find(s) else {
            return Err(ParseHandError::InvalidDealer);
        };

        let dealer = match s.as_bytes()[0].to_ascii_uppercase() {
            b'N' => Seat::North,
            b'E' => Seat::East,
            b'S' => Seat::South,
            b'W' => Seat::West,
            _ => unreachable!("Invalid dealer should have been caught by the regex"),
        };

        let hands: Result<Vec<_>, _> = s[tag.end()..]
            .split_whitespace()
            .map(Hand::from_str)
            .collect();

        let mut deal = Self(
            hands?
                .try_into()
                .map_err(|_| ParseHandError::NotFourHands)?,
        );
        deal.0.rotate_right(dealer as usize);
        Ok(deal)
    }
}
