#![doc = include_str!("../README.md")]
#![warn(missing_docs)]

/// Bidding and scoring
pub mod contract;
/// Deals and hands
pub mod deal;
/// Solver functions for double dummy problems
pub mod solver;

pub use contract::{Bid, Contract, Level, Penalty};
pub use deal::{Card, Deal, Hand, Holding, Rank, Seat, SeatFlags};
pub use solver::Solver;

use core::fmt::{self, Write as _};
use core::str::FromStr;
use thiserror::Error;

/// Denomination, a suit or notrump
///
/// We choose this representation over `Option<Suit>` because we are not sure if
/// the latter can be optimized to a single byte.
///
/// The order of the suits deviates from [`dds`][dds], but this order provides
/// natural ordering by deriving [`PartialOrd`] and [`Ord`].
///
/// [dds]: https://github.com/dds-bridge/dds
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(u8)]
pub enum Strain {
    /// ♣
    Clubs,
    /// ♦
    Diamonds,
    /// ♥
    Hearts,
    /// ♠
    Spades,
    /// NT, the strain not proposing a trump suit
    Notrump,
}

impl Strain {
    /// Whether this strain is a minor suit (clubs or diamonds)
    #[must_use]
    #[inline]
    pub const fn is_minor(self) -> bool {
        matches!(self, Self::Clubs | Self::Diamonds)
    }

    /// Whether this strain is a major suit (hearts or spades)
    #[must_use]
    #[inline]
    pub const fn is_major(self) -> bool {
        matches!(self, Self::Hearts | Self::Spades)
    }

    /// Whether this strain is a suit
    #[must_use]
    #[inline]
    pub const fn is_suit(self) -> bool {
        !matches!(self, Self::Notrump)
    }

    /// Whether this strain is notrump
    #[must_use]
    #[inline]
    pub const fn is_notrump(self) -> bool {
        matches!(self, Self::Notrump)
    }

    /// Convert to a [`Suit`], returning `None` for notrump
    #[must_use]
    #[inline]
    pub const fn suit(self) -> Option<Suit> {
        match self {
            Self::Clubs => Some(Suit::Clubs),
            Self::Diamonds => Some(Suit::Diamonds),
            Self::Hearts => Some(Suit::Hearts),
            Self::Spades => Some(Suit::Spades),
            Self::Notrump => None,
        }
    }

    /// Uppercase letter
    #[must_use]
    #[inline]
    pub const fn letter(self) -> char {
        match self {
            Self::Clubs => 'C',
            Self::Diamonds => 'D',
            Self::Hearts => 'H',
            Self::Spades => 'S',
            Self::Notrump => 'N',
        }
    }
}

impl fmt::Display for Strain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Clubs => f.write_char('♣'),
            Self::Diamonds => f.write_char('♦'),
            Self::Hearts => f.write_char('♥'),
            Self::Spades => f.write_char('♠'),
            Self::Notrump => f.write_str("NT"),
        }
    }
}

impl Strain {
    /// Strains in the ascending order, the order in this crate
    pub const ASC: [Self; 5] = [
        Self::Clubs,
        Self::Diamonds,
        Self::Hearts,
        Self::Spades,
        Self::Notrump,
    ];

    /// Strains in the descending order
    pub const DESC: [Self; 5] = [
        Self::Notrump,
        Self::Spades,
        Self::Hearts,
        Self::Diamonds,
        Self::Clubs,
    ];
}

/// A suit of playing cards
///
/// Suits are convertible to [`Strain`]s since suits form a subset of strains.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

    /// Uppercase letter
    #[must_use]
    #[inline]
    pub const fn letter(self) -> char {
        match self {
            Self::Clubs => 'C',
            Self::Diamonds => 'D',
            Self::Hearts => 'H',
            Self::Spades => 'S',
        }
    }
}

impl fmt::Display for Suit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(match self {
            Self::Clubs => '♣',
            Self::Diamonds => '♦',
            Self::Hearts => '♥',
            Self::Spades => '♠',
        })
    }
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

/// Unicode variation selectors that may appear after suit emojis
///
/// We want to ignore these suffixes when parsing suits.
const EMOJI_SELECTORS: [char; 2] = ['\u{FE0F}', '\u{FE0E}'];

/// Error returned when parsing a [`Suit`] fails
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("Invalid suit: expected one of C, D, H, S, ♣, ♦, ♥, ♠, ♧, ♢, ♡, ♤")]
pub struct ParseSuitError;

impl FromStr for Suit {
    type Err = ParseSuitError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s
            .to_ascii_uppercase()
            .as_str()
            .trim_end_matches(EMOJI_SELECTORS)
        {
            "C" | "♣" | "♧" => Ok(Self::Clubs),
            "D" | "♦" | "♢" => Ok(Self::Diamonds),
            "H" | "♥" | "♡" => Ok(Self::Hearts),
            "S" | "♠" | "♤" => Ok(Self::Spades),
            _ => Err(ParseSuitError),
        }
    }
}

/// Error returned when parsing a [`Strain`] fails
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("Invalid strain: expected one of C, D, H, S, N, NT, ♣, ♦, ♥, ♠, ♧, ♢, ♡, ♤")]
pub struct ParseStrainError;

impl FromStr for Strain {
    type Err = ParseStrainError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s
            .to_ascii_uppercase()
            .as_str()
            .trim_end_matches(EMOJI_SELECTORS)
        {
            "C" | "♣" | "♧" => Ok(Self::Clubs),
            "D" | "♦" | "♢" => Ok(Self::Diamonds),
            "H" | "♥" | "♡" => Ok(Self::Hearts),
            "S" | "♠" | "♤" => Ok(Self::Spades),
            "N" | "NT" => Ok(Self::Notrump),
            _ => Err(ParseStrainError),
        }
    }
}
