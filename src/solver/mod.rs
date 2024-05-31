#[cfg(test)]
mod test;

use crate::contract::Strain;
use crate::deal::{Deal, Seat};
use bitflags::bitflags;
use core::ffi::c_int;
use core::fmt;
use dds_bridge_sys as sys;
use thiserror::Error;

static THREAD_POOL: std::sync::Mutex<()> = std::sync::Mutex::new(());

/// Errors that can occur in the solver
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum Error {
    /// Success, no error
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_NO_FAULT) })]
    #[allow(clippy::cast_possible_wrap)]
    Success = sys::RETURN_NO_FAULT as i32,

    /// A general or unknown error
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_UNKNOWN_FAULT) })]
    UnknownFault = sys::RETURN_UNKNOWN_FAULT,

    /// Zero cards
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_ZERO_CARDS) })]
    ZeroCards = sys::RETURN_ZERO_CARDS,

    /// Target exceeds number of tricks
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_TARGET_TOO_HIGH) })]
    TargetTooHigh = sys::RETURN_TARGET_TOO_HIGH,

    /// Duplicate cards
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_DUPLICATE_CARDS) })]
    DuplicateCards = sys::RETURN_DUPLICATE_CARDS,

    /// Target tricks < 0
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_TARGET_WRONG_LO) })]
    NegativeTarget = sys::RETURN_TARGET_WRONG_LO,

    /// Target tricks > 13
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_TARGET_WRONG_HI) })]
    InvalidTarget = sys::RETURN_TARGET_WRONG_HI,

    /// Solving parameter < 1
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_SOLNS_WRONG_LO) })]
    LowSolvingParameter = sys::RETURN_SOLNS_WRONG_LO,

    /// Solving parameter > 3
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_SOLNS_WRONG_HI) })]
    HighSolvingParameter = sys::RETURN_SOLNS_WRONG_HI,

    /// Too many cards
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_TOO_MANY_CARDS) })]
    TooManyCards = sys::RETURN_TOO_MANY_CARDS,

    /// Wrong current suit or rank
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_SUIT_OR_RANK) })]
    CurrentSuitOrRank = sys::RETURN_SUIT_OR_RANK,

    /// Wrong played card
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_PLAYED_CARD) })]
    PlayedCard = sys::RETURN_PLAYED_CARD,

    /// Wrong card count
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_CARD_COUNT) })]
    CardCount = sys::RETURN_CARD_COUNT,

    /// Wrong thread index
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_THREAD_INDEX) })]
    ThreadIndex = sys::RETURN_THREAD_INDEX,

    /// Mode parameter < 0
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_MODE_WRONG_LO) })]
    NegativeModeParameter = sys::RETURN_MODE_WRONG_LO,

    /// Mode parameter > 2
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_MODE_WRONG_HI) })]
    HighModeParameter = sys::RETURN_MODE_WRONG_HI,

    /// Wrong trump suit
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_TRUMP_WRONG) })]
    Trump = sys::RETURN_TRUMP_WRONG,

    /// Wrong "first"
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_FIRST_WRONG) })]
    First = sys::RETURN_FIRST_WRONG,

    /// `AnalysePlay*()` family of functions.
    /// (a) Less than 0 or more than 52 cards supplied.
    /// (b) Invalid suit or rank supplied.
    /// (c) A played card is not held by the right player.
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_PLAY_FAULT) })]
    AnalysePlay = sys::RETURN_PLAY_FAULT,

    /// Invalid PBN
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_PBN_FAULT) })]
    PBN = sys::RETURN_PBN_FAULT,

    /// Too many boards
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_TOO_MANY_BOARDS) })]
    TooManyBoards = sys::RETURN_TOO_MANY_BOARDS,

    /// Cannot create a new thread
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_THREAD_CREATE) })]
    ThreadCreate = sys::RETURN_THREAD_CREATE,

    /// Failed to wait for a thread
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_THREAD_WAIT) })]
    ThreadWait = sys::RETURN_THREAD_WAIT,

    /// Missing threading system
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_THREAD_MISSING) })]
    ThreadMissing = sys::RETURN_THREAD_MISSING,

    /// No suit to solve
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_NO_SUIT) })]
    NoSuit = sys::RETURN_NO_SUIT,

    /// Too many tables
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_TOO_MANY_TABLES) })]
    TooManyTables = sys::RETURN_TOO_MANY_TABLES,

    /// Invalid chunk size
    #[error("{}", unsafe { core::str::from_utf8_unchecked(sys::TEXT_CHUNK_SIZE) })]
    ChunkSize = sys::RETURN_CHUNK_SIZE,
}

impl Error {
    /// Propagate a status code to an error
    ///
    /// - `x`: Arbitrary data to return if `status` is non-negative (success)
    /// - `status`: The status code from a DDS function
    ///
    /// # Errors
    /// An [`enum@Error`] specified by `status`
    pub const fn propagate<T: Copy>(x: T, status: i32) -> Result<T, Self> {
        match status {
            0.. => Ok(x),
            sys::RETURN_ZERO_CARDS => Err(Self::ZeroCards),
            sys::RETURN_TARGET_TOO_HIGH => Err(Self::TargetTooHigh),
            sys::RETURN_DUPLICATE_CARDS => Err(Self::DuplicateCards),
            sys::RETURN_TARGET_WRONG_LO => Err(Self::NegativeTarget),
            sys::RETURN_TARGET_WRONG_HI => Err(Self::InvalidTarget),
            sys::RETURN_SOLNS_WRONG_LO => Err(Self::LowSolvingParameter),
            sys::RETURN_SOLNS_WRONG_HI => Err(Self::HighSolvingParameter),
            sys::RETURN_TOO_MANY_CARDS => Err(Self::TooManyCards),
            sys::RETURN_SUIT_OR_RANK => Err(Self::CurrentSuitOrRank),
            sys::RETURN_PLAYED_CARD => Err(Self::PlayedCard),
            sys::RETURN_CARD_COUNT => Err(Self::CardCount),
            sys::RETURN_THREAD_INDEX => Err(Self::ThreadIndex),
            sys::RETURN_MODE_WRONG_LO => Err(Self::NegativeModeParameter),
            sys::RETURN_MODE_WRONG_HI => Err(Self::HighModeParameter),
            sys::RETURN_TRUMP_WRONG => Err(Self::Trump),
            sys::RETURN_FIRST_WRONG => Err(Self::First),
            sys::RETURN_PLAY_FAULT => Err(Self::AnalysePlay),
            sys::RETURN_PBN_FAULT => Err(Self::PBN),
            sys::RETURN_TOO_MANY_BOARDS => Err(Self::TooManyBoards),
            sys::RETURN_THREAD_CREATE => Err(Self::ThreadCreate),
            sys::RETURN_THREAD_WAIT => Err(Self::ThreadWait),
            sys::RETURN_THREAD_MISSING => Err(Self::ThreadMissing),
            sys::RETURN_NO_SUIT => Err(Self::NoSuit),
            sys::RETURN_TOO_MANY_TABLES => Err(Self::TooManyTables),
            sys::RETURN_CHUNK_SIZE => Err(Self::ChunkSize),
            _ => Err(Self::UnknownFault),
        }
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

/// Tricks that each seat can take as declarer for a strain
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

const fn make_row(row: [c_int; 4]) -> TricksRow {
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

/// Solve a single deal with [`sys::CalcDDtable`]
///
/// # Errors
/// An [`enum@Error`] propagated from DDS
pub fn solve_deal(deal: Deal) -> Result<TricksTable, Error> {
    let mut result = sys::ddTableResults::default();
    let _guard = THREAD_POOL.lock().map_err(|_| Error::ThreadWait)?;
    let status = unsafe { sys::CalcDDtable(deal.into(), &mut result) };
    Error::propagate(result.into(), status)
}

/// Solve deals with a single call of [`sys::CalcAllTables`]
///
/// - `deals`: A slice of deals to solve
/// - `flags`: Flags of strains to solve for
///
/// # Safety
/// `deals.len() * flags.bits().count_ones()` must not exceed
/// [`sys::MAXNOOFBOARDS`].
///
/// # Errors
/// An [`enum@Error`] propagated from DDS
pub unsafe fn solve_deal_segment(
    deals: &[Deal],
    flags: StrainFlags,
) -> Result<sys::ddTablesRes, Error> {
    debug_assert!(deals.len() * flags.bits().count_ones() as usize <= sys::MAXNOOFBOARDS as usize);
    let mut pack = sys::ddTableDeals {
        #[allow(clippy::cast_possible_wrap, clippy::cast_possible_truncation)]
        noOfTables: deals.len() as c_int,
        ..Default::default()
    };
    deals
        .iter()
        .copied()
        .enumerate()
        .for_each(|(i, deal)| pack.deals[i] = deal.into());

    let mut res = sys::ddTablesRes::default();
    let _guard = THREAD_POOL.lock().map_err(|_| Error::ThreadWait)?;
    let status = sys::CalcAllTables(
        &mut pack,
        -1,
        &mut [
            c_int::from(!flags.contains(StrainFlags::SPADES)),
            c_int::from(!flags.contains(StrainFlags::HEARTS)),
            c_int::from(!flags.contains(StrainFlags::DIAMONDS)),
            c_int::from(!flags.contains(StrainFlags::CLUBS)),
            c_int::from(!flags.contains(StrainFlags::NOTRUMP)),
        ][0],
        &mut res,
        &mut sys::allParResults::default(),
    );
    Error::propagate(res, status)
}

/// Solve deals in parallel for given strains
///
/// - `deals`: A slice of deals to solve
/// - `flags`: Flags of strains to solve for
///
/// # Errors
/// An [`enum@Error`] propagated from DDS
pub fn solve_deals(deals: &[Deal], flags: StrainFlags) -> Result<Vec<TricksTable>, Error> {
    let length = (sys::MAXNOOFBOARDS / flags.bits().count_ones()) as usize;
    let (q, r) = (deals.len() / length, deals.len() % length);
    let mut tables = Vec::new();

    for i in 0..q {
        let res = unsafe { solve_deal_segment(&deals[i * length..(i + 1) * length], flags) }?;
        tables.extend(res.results[..length].iter().copied().map(TricksTable::from));
    }

    if r > 0 {
        let res = unsafe { solve_deal_segment(&deals[q * length..], flags) }?;
        tables.extend(res.results[..r].iter().copied().map(TricksTable::from));
    }

    Ok(tables)
}

/// Target tricks and number of solutions to find
///
/// This enum corresponds to a tuple of `target` and `solutions` in
/// [`sys::SolveBoard`].  The `target` tricks given as an associated value must
/// be in the range of `-1..=13`, where `-1` instructs the solver to find cards
/// that give the most tricks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    /// Find any card that fulfills the target
    ///
    /// - `0..=13`: Find any card scoring at least `target` tricks
    /// - `-1`: Find any card scoring the most tricks
    Any(i8),

    /// Find all cards that fulfill the target
    ///
    /// - `0..=13`: Find all cards scoring at least `target` tricks
    /// - `-1`: Find all cards scoring the most tricks
    All(i8),

    /// Solve for all legal plays
    ///
    /// Cards are sorted with their scores in descending order.
    Legal,
}

impl Target {
    /// Get the `target` argument for [`sys::SolveBoard`]
    #[must_use]
    pub const fn target(self) -> c_int {
        match self {
            Self::Any(target) | Self::All(target) => target as c_int,
            Self::Legal => -1,
        }
    }

    /// Get the `solutions` argument for [`sys::SolveBoard`]
    #[must_use]
    pub const fn solutions(self) -> c_int {
        match self {
            Self::Any(_) => 1,
            Self::All(_) => 2,
            Self::Legal => 3,
        }
    }
}

struct Board {
    deal: Deal,
}

impl From<Board> for sys::deal {
    fn from(board: Board) -> Self {
        Self {
            remainCards: sys::ddTableDeal::from(board.deal).cards,
            ..Default::default()
        }
    }
}

fn solve_board(board: Board, target: Target) -> Result<sys::futureTricks, Error> {
    let mut result = sys::futureTricks::default();
    let status = unsafe {
        sys::SolveBoard(
            board.into(),
            target.target(),
            target.solutions(),
            0,
            &mut result,
            0,
        )
    };
    Error::propagate(result, status)
}
