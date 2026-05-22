//! Trick counts and their conversions to/from DDS FFI types

use contract_bridge::deal::Builder;
use contract_bridge::seat::Seat;
use contract_bridge::{Strain, Suit};

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
/// A validated newtype over `u8`, analogous to [`Level`](contract_bridge::contract::Level)
/// (1..=7) and [`Rank`](contract_bridge::hand::Rank) (2..=14). Appears as the per-seat
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

/// Convert a [`Strain`] to its index in [`dds_bridge_sys`]
#[must_use]
const fn strain_to_sys(strain: Strain) -> usize {
    match strain {
        Strain::Spades => 0,
        Strain::Hearts => 1,
        Strain::Diamonds => 2,
        Strain::Clubs => 3,
        Strain::Notrump => 4,
    }
}

impl From<sys::DdTableResults> for TrickCountTable {
    fn from(table: sys::DdTableResults) -> Self {
        use super::ffi::trick_count_from_sys;
        let row = |r: [c_int; 4]| {
            TrickCountRow::new(
                trick_count_from_sys(r[0]).get(),
                trick_count_from_sys(r[1]).get(),
                trick_count_from_sys(r[2]).get(),
                trick_count_from_sys(r[3]).get(),
            )
        };

        Self([
            row(table.res_table[strain_to_sys(Strain::Clubs)]),
            row(table.res_table[strain_to_sys(Strain::Diamonds)]),
            row(table.res_table[strain_to_sys(Strain::Hearts)]),
            row(table.res_table[strain_to_sys(Strain::Spades)]),
            row(table.res_table[strain_to_sys(Strain::Notrump)]),
        ])
    }
}

impl From<TrickCountTable> for sys::DdTableResults {
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
            res_table: [
                make_row(table[Strain::Spades]),
                make_row(table[Strain::Hearts]),
                make_row(table[Strain::Diamonds]),
                make_row(table[Strain::Clubs]),
                make_row(table[Strain::Notrump]),
            ],
        }
    }
}

/// Convert a [`Builder`] into the DDS `DdTableDeal`.  `Builder` is
/// unvalidated, so prefer the [`FullDeal`] or [`PartialDeal`] entry points
/// via [`dd_table_deal_from`].
#[must_use]
pub(crate) fn dd_table_deal_from_builder(builder: Builder) -> sys::DdTableDeal {
    sys::DdTableDeal {
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

/// Convert a validated deal (either [`FullDeal`] or [`PartialDeal`]) into a
/// DDS `DdTableDeal`.
#[must_use]
pub(crate) fn dd_table_deal_from(deal: impl Into<Builder>) -> sys::DdTableDeal {
    dd_table_deal_from_builder(deal.into())
}
