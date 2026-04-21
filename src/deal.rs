#[cfg(feature = "serde")]
mod serde_;

use crate::hand::{Hand, ParseHandError};
use core::fmt::{self, Write as _};
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

/// An error which can be returned when parsing a [`Subset`] or [`FullDeal`]
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
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

    /// The deal is not a valid [`Subset`]: some hand has more than 13 cards or
    /// two hands share a card
    #[error("The deal is not a valid subset (>13 cards per hand or overlapping hands)")]
    InvalidSubset,

    /// The deal is not a [`FullDeal`]: some hand does not have exactly 13 cards
    #[error("The deal is not a full deal (each hand must have exactly 13 cards)")]
    NotFullDeal,
}

/// A loose deal builder — any combination of four hands, no invariants
///
/// Use `Builder` to construct a deal incrementally.  Convert it into a
/// [`Subset`] or [`FullDeal`] (via the inherent [`build_subset`] /
/// [`build_full`] methods, or via [`TryFrom`]) once the hands are finalized.
/// `Builder` is the only deal type that exposes [`IndexMut`](ops::IndexMut)
/// for in-place mutation.
///
/// [`build_subset`]: Builder::build_subset
/// [`build_full`]: Builder::build_full
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Builder([Hand; 4]);

impl IntoIterator for Builder {
    type Item = Hand;
    type IntoIter = core::array::IntoIter<Hand, 4>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl ops::Index<Seat> for Builder {
    type Output = Hand;

    #[inline]
    fn index(&self, seat: Seat) -> &Hand {
        &self.0[seat as usize]
    }
}

impl ops::IndexMut<Seat> for Builder {
    #[inline]
    fn index_mut(&mut self, seat: Seat) -> &mut Hand {
        &mut self.0[seat as usize]
    }
}

impl Builder {
    /// Empty builder — all four hands empty
    pub const EMPTY: Self = Self([Hand::EMPTY; 4]);

    /// Construct a builder from four hands in seat order
    #[must_use]
    pub const fn new(north: Hand, east: Hand, south: Hand, west: Hand) -> Self {
        Self([north, east, south, west])
    }

    /// Try to convert this builder into a [`Subset`], validating that each
    /// hand has at most 13 cards and the hands are pairwise disjoint.  On
    /// failure the input is returned unchanged as the error.
    ///
    /// # Errors
    ///
    /// Returns `self` unchanged if the builder is not a valid subset.
    pub fn build_subset(self) -> Result<Subset, Self> {
        let mut seen = Hand::EMPTY;
        for hand in self.0 {
            if hand.len() > 13 || hand & seen != Hand::EMPTY {
                return Err(self);
            }
            seen |= hand;
        }
        Ok(Subset(self))
    }

    /// Try to convert this builder into a [`FullDeal`], validating that each
    /// hand has exactly 13 cards and the hands are pairwise disjoint.  On
    /// failure the input is returned unchanged as the error.
    ///
    /// # Errors
    ///
    /// Returns `self` unchanged if the builder is not a valid full deal.
    pub fn build_full(self) -> Result<FullDeal, Self> {
        match self.build_subset() {
            Ok(subset) if subset.len() == 52 => Ok(FullDeal(subset.0)),
            Ok(subset) => Err(subset.0),
            Err(builder) => Err(builder),
        }
    }
}

/// A validated subset of a bridge deal
///
/// Invariants: each hand holds at most 13 cards, and the four hands are
/// pairwise disjoint.  Construct via [`Builder::build_subset`],
/// [`TryFrom<Builder>`], the infallible widening from a [`FullDeal`], or by
/// parsing a PBN-ish string.
///
/// `Subset` is read-only: it exposes [`Index<Seat>`](ops::Index) but not
/// [`IndexMut`](ops::IndexMut).  To mutate, widen back to a [`Builder`].
///
/// Parses the [PBN] deal format with relaxed per-hand size —
/// `<dealer>:<hand> <hand> <hand> <hand>` — where each hand is four
/// dot-separated holdings ordered spades, hearts, diamonds, clubs.  Holdings
/// may be empty or contain `x` spot cards for unknown ranks.  Hands are
/// listed clockwise starting from the dealer.
///
/// [PBN]: https://www.tistis.nl/pbn/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Subset(Builder);

impl ops::Index<Seat> for Subset {
    type Output = Hand;

    #[inline]
    fn index(&self, seat: Seat) -> &Hand {
        &self.0[seat]
    }
}

impl Subset {
    /// Empty subset — all four hands empty
    pub const EMPTY: Self = Self(Builder::EMPTY);

    /// Collect all cards in the subset into a single hand
    #[must_use]
    pub fn collected(&self) -> Hand {
        self.0.into_iter().fold(Hand::EMPTY, |a, h| a | h)
    }

    /// Total number of cards across the four hands
    #[must_use]
    pub fn len(&self) -> usize {
        self.collected().len()
    }

    /// Whether the subset has no cards at all
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.collected().is_empty()
    }

    /// PBN-compatible display from a seat's perspective
    #[must_use]
    pub fn display(&self, seat: Seat) -> impl fmt::Display + use<> {
        DisplayAt {
            builder: self.0,
            seat,
        }
    }
}

impl From<Subset> for Builder {
    #[inline]
    fn from(subset: Subset) -> Self {
        subset.0
    }
}

impl TryFrom<Builder> for Subset {
    type Error = Builder;

    #[inline]
    fn try_from(builder: Builder) -> Result<Self, Self::Error> {
        builder.build_subset()
    }
}

impl FromStr for Subset {
    type Err = ParseDealError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_pbn(s)?
            .build_subset()
            .map_err(|_| ParseDealError::InvalidSubset)
    }
}

/// A full bridge deal — exactly 13 cards per hand, 52 total
///
/// Invariants: each of the four hands contains exactly 13 cards, and the
/// hands partition the full 52-card deck.  Construct via
/// [`Builder::build_full`], [`TryFrom<Builder>`], [`TryFrom<Subset>`], or by
/// parsing a PBN string.
///
/// `FullDeal` is read-only.  Parses the [PBN] deal format:
/// `<dealer>:<hand> <hand> <hand> <hand>`, where each hand is four
/// dot-separated holdings ordered spades, hearts, diamonds, clubs.  Hands
/// are listed clockwise starting from the dealer.
///
/// # Examples
///
/// ```
/// use dds_bridge::{FullDeal, Rank, Seat, Suit};
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let deal: FullDeal = "N:.63.AKQ987.A9732 A8654.KQ5.T.QJT6 \
///                       J973.J98742.3.K4 KQT2.AT.J6542.85".parse()?;
/// assert!(deal[Seat::East][Suit::Spades].contains(Rank::A));
/// # Ok(())
/// # }
/// ```
///
/// [PBN]: https://www.tistis.nl/pbn/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FullDeal(Builder);

impl ops::Index<Seat> for FullDeal {
    type Output = Hand;

    #[inline]
    fn index(&self, seat: Seat) -> &Hand {
        &self.0[seat]
    }
}

impl IntoIterator for FullDeal {
    type Item = Hand;
    type IntoIter = core::array::IntoIter<Hand, 4>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FullDeal {
    /// PBN-compatible display from a seat's perspective
    #[must_use]
    pub fn display(&self, seat: Seat) -> impl fmt::Display + use<> {
        DisplayAt {
            builder: self.0,
            seat,
        }
    }
}

impl From<FullDeal> for Builder {
    #[inline]
    fn from(deal: FullDeal) -> Self {
        deal.0
    }
}

impl From<FullDeal> for Subset {
    #[inline]
    fn from(deal: FullDeal) -> Self {
        Self(deal.0)
    }
}

impl TryFrom<Builder> for FullDeal {
    type Error = Builder;

    #[inline]
    fn try_from(builder: Builder) -> Result<Self, Self::Error> {
        builder.build_full()
    }
}

impl TryFrom<Subset> for FullDeal {
    type Error = Subset;

    #[inline]
    fn try_from(subset: Subset) -> Result<Self, Self::Error> {
        match subset.0.build_full() {
            Ok(full) => Ok(full),
            Err(builder) => Err(Subset(builder)),
        }
    }
}

impl FromStr for FullDeal {
    type Err = ParseDealError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_pbn(s)?
            .build_full()
            .map_err(|_| ParseDealError::NotFullDeal)
    }
}

/// Shared PBN deal parser: reads `<dealer>:<hand> <hand> <hand> <hand>` and
/// returns a `Builder` with hands rotated so seat index 0 is North.
fn parse_pbn(s: &str) -> Result<Builder, ParseDealError> {
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

    let mut builder = Builder(
        hands?
            .try_into()
            .map_err(|_| ParseDealError::NotFourHands)?,
    );
    builder.0.rotate_right(dealer as usize);
    Ok(builder)
}

/// Shared PBN-compatible `Display` helper for [`Subset`] and [`FullDeal`]
struct DisplayAt {
    builder: Builder,
    seat: Seat,
}

impl fmt::Display for DisplayAt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char(self.seat.letter())?;
        f.write_char(':')?;

        self.builder[self.seat].fmt(f)?;
        f.write_char(' ')?;

        self.builder[self.seat.lho()].fmt(f)?;
        f.write_char(' ')?;

        self.builder[self.seat.partner()].fmt(f)?;
        f.write_char(' ')?;

        self.builder[self.seat.rho()].fmt(f)
    }
}
