//! Table positions and bitsets over them.
//!
//! [`Seat`] names one of the four positions at the table ([`North`], [`East`],
//! [`South`], [`West`]) in dealing order.  It is the indexing key for the deal
//! containers in [`crate::deal`], but it is also useful on its own — e.g. as a
//! dealer tag, as the declarer of a [`Contract`](crate::Contract), or as the
//! leader of a trick.  Parses case-insensitively from full names (`"North"`)
//! or single letters (`"N"`).
//!
//! [`SeatFlags`] is a [`bitflags`] bitset over seats with named constants for
//! the empty and full sets ([`SeatFlags::EMPTY`], [`SeatFlags::ALL`]) and the
//! two partnerships ([`SeatFlags::NS`], [`SeatFlags::EW`]).  A single `Seat`
//! converts into a singleton `SeatFlags` via [`From<Seat>`].
//!
//! When the `serde` feature is enabled, `Seat` derives `Serialize`/
//! `Deserialize` using the standard enum-variant encoding, and `SeatFlags`
//! derives the `bitflags` crate's serde encoding.
//!
//! [`North`]: Seat::North
//! [`East`]: Seat::East
//! [`South`]: Seat::South
//! [`West`]: Seat::West

use core::fmt::{self, Write as _};
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
