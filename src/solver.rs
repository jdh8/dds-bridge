use crate::contract::Strain;
use crate::deal::{Deal, Seat};
use bitflags::bitflags;
use core::fmt;
use dds_bridge_sys as sys;

/// Tricks that each seat can take as declarer for a strain
#[derive(Debug, Clone, Copy)]
pub struct TricksRow(u16);

impl TricksRow {
    /// Create a new row from the number of tricks each seat can take
    #[must_use]
    pub const fn new(n: u8, e: u8, s: u8, w: u8) -> Self {
        Self(
            (n as u16) << (4 * Seat::North as u8)
                | (e as u16) << (4 * Seat::East as u8)
                | (s as u16) << (4 * Seat::South as u8)
                | (w as u16) << (4 * Seat::West as u8),
        )
    }

    /// Get the number of tricks a seat can take as declarer
    #[must_use]
    pub const fn at(self, seat: Seat) -> u8 {
        (self.0 >> (4 * seat as u8) & 0xF) as u8
    }
}

impl fmt::Display for TricksRow {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:X}{:X}{:X}{:X}",
            self.at(Seat::North),
            self.at(Seat::East),
            self.at(Seat::South),
            self.at(Seat::West)
        )
    }
}

/// Tricks that each seat can take as declarer for all strains
#[derive(Debug, Clone, Copy)]
pub struct TricksTable([TricksRow; 5]);

impl core::ops::Index<Strain> for TricksTable {
    type Output = TricksRow;

    fn index(&self, strain: Strain) -> &TricksRow {
        &self.0[strain as usize]
    }
}

impl fmt::Display for TricksTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}{}{}{}",
            self.0[0], self.0[1], self.0[2], self.0[3], self.0[4]
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

const fn make_row(row: [i32; 4]) -> TricksRow {
    #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
    TricksRow::new(row[0] as u8, row[1] as u8, row[2] as u8, row[3] as u8)
}

impl From<sys::ddTableResults> for TricksTable {
    fn from(table: sys::ddTableResults) -> Self {
        Self([
            make_row(table.resTable[Strain::Spades as usize]),
            make_row(table.resTable[Strain::Hearts as usize]),
            make_row(table.resTable[Strain::Diamonds as usize]),
            make_row(table.resTable[Strain::Clubs as usize]),
            make_row(table.resTable[Strain::Notrump as usize]),
        ])
    }
}

impl From<Deal> for sys::ddTableDeal {
    fn from(deal: Deal) -> Self {
        Self {
            cards: [
                [
                    deal[Seat::North][Strain::Spades].to_bits().into(),
                    deal[Seat::North][Strain::Hearts].to_bits().into(),
                    deal[Seat::North][Strain::Diamonds].to_bits().into(),
                    deal[Seat::North][Strain::Clubs].to_bits().into(),
                ],
                [
                    deal[Seat::East][Strain::Spades].to_bits().into(),
                    deal[Seat::East][Strain::Hearts].to_bits().into(),
                    deal[Seat::East][Strain::Diamonds].to_bits().into(),
                    deal[Seat::East][Strain::Clubs].to_bits().into(),
                ],
                [
                    deal[Seat::South][Strain::Spades].to_bits().into(),
                    deal[Seat::South][Strain::Hearts].to_bits().into(),
                    deal[Seat::South][Strain::Diamonds].to_bits().into(),
                    deal[Seat::South][Strain::Clubs].to_bits().into(),
                ],
                [
                    deal[Seat::West][Strain::Spades].to_bits().into(),
                    deal[Seat::West][Strain::Hearts].to_bits().into(),
                    deal[Seat::West][Strain::Diamonds].to_bits().into(),
                    deal[Seat::West][Strain::Clubs].to_bits().into(),
                ],
            ],
        }
    }
}

/// Solve a deal segment with [`sys::CalcAllTables`]
///
/// - `deals`: A slice of deals to solve
/// - `filter`: Reverse filter in [spades, hearts, diamonds, clubs, notrump]
///
/// Note that zero entries in the filter means to solve for that strain.  For
/// example, `[0, 0, 1, 1, 1]` means to solve for major suits only.
///
/// # Safety
/// The length of `deals` times the number of strains to solve for must not
/// exceed [`sys::MAXNOOFBOARDS`].
#[must_use]
unsafe fn solve_deal_segment(deals: &[Deal], mut filter: [i32; 5]) -> sys::ddTablesRes {
    let mut res = sys::ddTablesRes::default();

    debug_assert!(
        deals.len() * filter.iter().copied().filter(|&i| i == 0).count() <= res.results.len()
    );

    let mut pack = sys::ddTableDeals {
        #[allow(clippy::cast_possible_wrap, clippy::cast_possible_truncation)]
        noOfTables: deals.len() as i32,
        ..Default::default()
    };

    deals
        .iter()
        .copied()
        .enumerate()
        .for_each(|(i, deal)| pack.deals[i] = deal.into());

    sys::CalcAllTables(
        &mut pack,
        -1,
        &mut filter[0],
        &mut res,
        core::ptr::null_mut(),
    );

    res
}

/// Solve deals in parallel for given strains
///
/// - `deals`: A slice of deals to solve
/// - `flags`: Flags of strains to solve for
#[must_use]
pub fn solve_deals(deals: &[Deal], flags: StrainFlags) -> Vec<TricksTable> {
    let filter = [
        i32::from(!flags.contains(StrainFlags::SPADES)),
        i32::from(!flags.contains(StrainFlags::HEARTS)),
        i32::from(!flags.contains(StrainFlags::DIAMONDS)),
        i32::from(!flags.contains(StrainFlags::CLUBS)),
        i32::from(!flags.contains(StrainFlags::NOTRUMP)),
    ];

    let length = (sys::MAXNOOFBOARDS / flags.bits().count_ones()) as usize;
    let (q, r) = (deals.len() / length, deals.len() % length);
    let mut tables = Vec::new();

    for i in 0..q {
        let res = unsafe { solve_deal_segment(&deals[i * length..(i + 1) * length], filter) };
        tables.extend(res.results[..length].iter().copied().map(TricksTable::from));
    }

    if r > 0 {
        let res = unsafe { solve_deal_segment(&deals[q * length..], filter) };
        tables.extend(res.results[..r].iter().copied().map(TricksTable::from));
    }

    tables
}
