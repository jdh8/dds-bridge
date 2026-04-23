//! Trick counts and their conversions to/from DDS FFI types

use crate::deal::{Builder, FullDeal, PartialDeal};
use crate::seat::Seat;
use crate::{Strain, Suit};

use dds_bridge_sys as sys;
use thiserror::Error;

use core::ffi::c_int;
use core::fmt;

/// Error returned when a trick count is outside `0..=13`
///
/// Produced by both [`TrickCount::try_new`] and [`TrickCountRow::try_new`].
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, Hash)]
#[error("trick count must be in 0..=13")]
pub struct InvalidTrickCount;

/// A number of tricks in `0..=13`
///
/// A validated newtype over `u8`, analogous to [`Level`](crate::contract::Level)
/// (1..=7) and [`Rank`](crate::hand::Rank) (2..=14). Appears as the per-seat
/// value returned by [`TrickCountRow::get`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[repr(transparent)]
pub struct TrickCount(u8);

impl TrickCount {
    /// Create a new trick count
    ///
    /// # Panics
    ///
    /// When `n` is outside `0..=13`. In const contexts, this is a compile-time
    /// error.
    #[must_use]
    #[inline]
    pub const fn new(n: u8) -> Self {
        match Self::try_new(n) {
            Ok(tc) => tc,
            Err(_) => panic!("trick count must be in 0..=13"),
        }
    }

    /// Try to create a new trick count
    ///
    /// # Errors
    ///
    /// When `n` is outside `0..=13`.
    #[inline]
    pub const fn try_new(n: u8) -> Result<Self, InvalidTrickCount> {
        if n > 13 {
            return Err(InvalidTrickCount);
        }
        Ok(Self(n))
    }

    /// Get the underlying `u8`
    #[must_use]
    #[inline]
    pub const fn get(self) -> u8 {
        self.0
    }
}

impl From<TrickCount> for u8 {
    #[inline]
    fn from(tc: TrickCount) -> Self {
        tc.0
    }
}

impl From<TrickCount> for usize {
    #[inline]
    fn from(tc: TrickCount) -> Self {
        tc.0 as Self
    }
}

impl fmt::Display for TrickCount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Tricks that each seat can take as declarer for a strain
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[repr(transparent)]
pub struct TrickCountRow(u16);

impl TrickCountRow {
    /// Create a new row from the number of tricks each seat can take
    ///
    /// # Panics
    ///
    /// When any value is outside `0..=13`.  In const contexts, this is a
    /// compile-time error.
    #[must_use]
    #[inline]
    pub const fn new(n: u8, e: u8, s: u8, w: u8) -> Self {
        match Self::try_new(n, e, s, w) {
            Ok(row) => row,
            Err(_) => panic!("trick count must be in 0..=13"),
        }
    }

    /// Try to create a new row from the number of tricks each seat can take
    ///
    /// # Errors
    ///
    /// When any value is outside `0..=13`.
    #[inline]
    pub const fn try_new(n: u8, e: u8, s: u8, w: u8) -> Result<Self, InvalidTrickCount> {
        if n > 13 || e > 13 || s > 13 || w > 13 {
            return Err(InvalidTrickCount);
        }
        Ok(Self(
            (n as u16) << (4 * Seat::North as u8)
                | (e as u16) << (4 * Seat::East as u8)
                | (s as u16) << (4 * Seat::South as u8)
                | (w as u16) << (4 * Seat::West as u8),
        ))
    }

    /// Get the number of tricks a seat can take as declarer
    #[must_use]
    pub const fn get(self, seat: Seat) -> TrickCount {
        TrickCount((self.0 >> (4 * seat as u8) & 0xF) as u8)
    }

    /// Hexadecimal representation from a seat's perspective
    #[must_use]
    pub const fn hex(self, seat: Seat) -> TrickCountRowHex {
        TrickCountRowHex { row: self, seat }
    }
}

/// Hexadecimal view of a [`TrickCountRow`] from a seat's perspective
///
/// Returned by [`TrickCountRow::hex`]. Formats as four hex digits — the tricks
/// taken by the seat, its LHO, its partner, and its RHO — via the
/// [`UpperHex`](fmt::UpperHex) impl.
#[derive(Debug, Clone, Copy)]
pub struct TrickCountRowHex {
    row: TrickCountRow,
    seat: Seat,
}

impl fmt::UpperHex for TrickCountRowHex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:X}{:X}{:X}{:X}",
            self.row.get(self.seat).get(),
            self.row.get(self.seat.lho()).get(),
            self.row.get(self.seat.partner()).get(),
            self.row.get(self.seat.rho()).get(),
        )
    }
}

/// Tricks that each seat can take as declarer for all strains
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[repr(transparent)]
pub struct TrickCountTable(pub [TrickCountRow; 5]);

impl core::ops::Index<Strain> for TrickCountTable {
    type Output = TrickCountRow;

    fn index(&self, strain: Strain) -> &TrickCountRow {
        &self.0[strain as usize]
    }
}

impl TrickCountTable {
    /// Hexadecimal representation from a seat's perspective
    #[must_use]
    pub const fn hex<T: AsRef<[Strain]>>(self, seat: Seat, strains: T) -> TrickCountTableHex<T> {
        TrickCountTableHex {
            table: self,
            seat,
            strains,
        }
    }
}

/// Hexadecimal view of a [`TrickCountTable`] from a seat's perspective
///
/// Returned by [`TrickCountTable::hex`]. Formats as one [`TrickCountRowHex`]
/// per strain in the supplied slice, concatenated, via the
/// [`UpperHex`](fmt::UpperHex) impl.
#[derive(Debug, Clone, Copy)]
pub struct TrickCountTableHex<T: AsRef<[Strain]>> {
    table: TrickCountTable,
    seat: Seat,
    strains: T,
}

impl<T: AsRef<[Strain]>> fmt::UpperHex for TrickCountTableHex<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for &strain in self.strains.as_ref() {
            self.table[strain].hex(self.seat).fmt(f)?;
        }
        Ok(())
    }
}

impl Strain {
    /// Convert to the index in [`dds_bridge_sys`]
    #[must_use]
    const fn to_sys(self) -> usize {
        match self {
            Self::Spades => 0,
            Self::Hearts => 1,
            Self::Diamonds => 2,
            Self::Clubs => 3,
            Self::Notrump => 4,
        }
    }
}

impl From<sys::ddTableResults> for TrickCountTable {
    fn from(table: sys::ddTableResults) -> Self {
        const fn make_row(row: [c_int; 4]) -> TrickCountRow {
            #[allow(clippy::cast_sign_loss)]
            TrickCountRow::new(
                (row[0] & 0xF) as u8,
                (row[1] & 0xF) as u8,
                (row[2] & 0xF) as u8,
                (row[3] & 0xF) as u8,
            )
        }

        Self([
            make_row(table.resTable[Strain::Clubs.to_sys()]),
            make_row(table.resTable[Strain::Diamonds.to_sys()]),
            make_row(table.resTable[Strain::Hearts.to_sys()]),
            make_row(table.resTable[Strain::Spades.to_sys()]),
            make_row(table.resTable[Strain::Notrump.to_sys()]),
        ])
    }
}

impl From<TrickCountTable> for sys::ddTableResults {
    fn from(table: TrickCountTable) -> Self {
        const fn make_row(row: TrickCountRow) -> [c_int; 4] {
            [
                row.get(Seat::North).get() as c_int,
                row.get(Seat::East).get() as c_int,
                row.get(Seat::South).get() as c_int,
                row.get(Seat::West).get() as c_int,
            ]
        }

        Self {
            resTable: [
                make_row(table[Strain::Spades]),
                make_row(table[Strain::Hearts]),
                make_row(table[Strain::Diamonds]),
                make_row(table[Strain::Clubs]),
                make_row(table[Strain::Notrump]),
            ],
        }
    }
}

/// FFI converter for a [`Builder`].  Used internally by [`FullDeal`] and
/// [`PartialDeal`] converters; `Builder` itself is unvalidated so prefer those.
impl From<Builder> for sys::ddTableDeal {
    fn from(builder: Builder) -> Self {
        Self {
            cards: Seat::ALL.map(|seat| {
                let hand = builder[seat];
                [
                    hand[Suit::Spades].to_bits().into(),
                    hand[Suit::Hearts].to_bits().into(),
                    hand[Suit::Diamonds].to_bits().into(),
                    hand[Suit::Clubs].to_bits().into(),
                ]
            }),
        }
    }
}

impl From<FullDeal> for sys::ddTableDeal {
    #[inline]
    fn from(deal: FullDeal) -> Self {
        Builder::from(deal).into()
    }
}

impl From<PartialDeal> for sys::ddTableDeal {
    #[inline]
    fn from(subset: PartialDeal) -> Self {
        Builder::from(subset).into()
    }
}
