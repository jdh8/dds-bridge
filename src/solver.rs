use crate::contract::{Contract, Penalty};
use crate::deal::{Card, Deal, Holding, Rank, Seat};
use crate::{Strain, Suit};

use arrayvec::ArrayVec;
use dds_bridge_sys as sys;
use parking_lot::Mutex;
use semver::Version;
use thiserror::Error;

use core::ffi::{CStr, c_char, c_int};
use core::fmt;
use core::mem::MaybeUninit;
use core::ops::BitOr as _;
use core::str::FromStr;
use std::sync::LazyLock;

/// # Panics
///
/// Panics if `status` is negative, which indicates an error in DDS.  The panic
/// message is a human-readable description of the error code returned by DDS.
const fn check(status: i32) {
    let msg: &[u8] = match status {
        0.. => return,
        sys::RETURN_ZERO_CARDS => sys::TEXT_ZERO_CARDS,
        sys::RETURN_TARGET_TOO_HIGH => sys::TEXT_TARGET_TOO_HIGH,
        sys::RETURN_DUPLICATE_CARDS => sys::TEXT_DUPLICATE_CARDS,
        sys::RETURN_TARGET_WRONG_LO => sys::TEXT_TARGET_WRONG_LO,
        sys::RETURN_TARGET_WRONG_HI => sys::TEXT_TARGET_WRONG_HI,
        sys::RETURN_SOLNS_WRONG_LO => sys::TEXT_SOLNS_WRONG_LO,
        sys::RETURN_SOLNS_WRONG_HI => sys::TEXT_SOLNS_WRONG_HI,
        sys::RETURN_TOO_MANY_CARDS => sys::TEXT_TOO_MANY_CARDS,
        sys::RETURN_SUIT_OR_RANK => sys::TEXT_SUIT_OR_RANK,
        sys::RETURN_PLAYED_CARD => sys::TEXT_PLAYED_CARD,
        sys::RETURN_CARD_COUNT => sys::TEXT_CARD_COUNT,
        sys::RETURN_THREAD_INDEX => sys::TEXT_THREAD_INDEX,
        sys::RETURN_MODE_WRONG_LO => sys::TEXT_MODE_WRONG_LO,
        sys::RETURN_MODE_WRONG_HI => sys::TEXT_MODE_WRONG_HI,
        sys::RETURN_TRUMP_WRONG => sys::TEXT_TRUMP_WRONG,
        sys::RETURN_FIRST_WRONG => sys::TEXT_FIRST_WRONG,
        sys::RETURN_PLAY_FAULT => sys::TEXT_PLAY_FAULT,
        sys::RETURN_PBN_FAULT => sys::TEXT_PBN_FAULT,
        sys::RETURN_TOO_MANY_BOARDS => sys::TEXT_TOO_MANY_BOARDS,
        sys::RETURN_THREAD_CREATE => sys::TEXT_THREAD_CREATE,
        sys::RETURN_THREAD_WAIT => sys::TEXT_THREAD_WAIT,
        sys::RETURN_THREAD_MISSING => sys::TEXT_THREAD_MISSING,
        sys::RETURN_NO_SUIT => sys::TEXT_NO_SUIT,
        sys::RETURN_TOO_MANY_TABLES => sys::TEXT_TOO_MANY_TABLES,
        sys::RETURN_CHUNK_SIZE => sys::TEXT_CHUNK_SIZE,
        _ => sys::TEXT_UNKNOWN_FAULT,
    };
    // SAFETY: Error messages are ASCII literals in the C++ code of DDS.
    panic!("{}", unsafe { core::str::from_utf8_unchecked(msg) });
}

bitflags::bitflags! {
    /// Flags for the solver to solve for a strain
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[repr(transparent)]
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
    pub const fn get(self, seat: Seat) -> u8 {
        (self.0 >> (4 * seat as u8) & 0xF) as u8
    }

    /// Hexadecimal representation from a seat's perspective
    #[must_use]
    pub const fn hex(self, seat: Seat) -> TricksRowHex {
        TricksRowHex { row: self, seat }
    }
}

/// Hexadecimal view of a [`TricksRow`] from a seat's perspective
///
/// Returned by [`TricksRow::hex`]. Formats as four hex digits — the tricks
/// taken by the seat, its LHO, its partner, and its RHO — via the
/// [`UpperHex`](fmt::UpperHex) impl.
#[derive(Debug, Clone, Copy)]
pub struct TricksRowHex {
    row: TricksRow,
    seat: Seat,
}

impl fmt::UpperHex for TricksRowHex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:X}{:X}{:X}{:X}",
            self.row.get(self.seat),
            self.row.get(self.seat.lho()),
            self.row.get(self.seat.partner()),
            self.row.get(self.seat.rho()),
        )
    }
}

/// Tricks that each seat can take as declarer for all strains
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[repr(transparent)]
pub struct TricksTable(pub [TricksRow; 5]);

impl core::ops::Index<Strain> for TricksTable {
    type Output = TricksRow;

    fn index(&self, strain: Strain) -> &TricksRow {
        &self.0[strain as usize]
    }
}

impl TricksTable {
    /// Hexadecimal representation from a seat's perspective
    #[must_use]
    pub const fn hex<T: AsRef<[Strain]>>(self, seat: Seat, strains: T) -> TricksTableHex<T> {
        TricksTableHex {
            table: self,
            seat,
            strains,
        }
    }
}

/// Hexadecimal view of a [`TricksTable`] from a seat's perspective
///
/// Returned by [`TricksTable::hex`]. Formats as one [`TricksRowHex`] per
/// strain in the supplied slice, concatenated, via the
/// [`UpperHex`](fmt::UpperHex) impl.
#[derive(Debug, Clone, Copy)]
pub struct TricksTableHex<T: AsRef<[Strain]>> {
    table: TricksTable,
    seat: Seat,
    strains: T,
}

impl<T: AsRef<[Strain]>> fmt::UpperHex for TricksTableHex<T> {
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

impl From<sys::ddTableResults> for TricksTable {
    fn from(table: sys::ddTableResults) -> Self {
        const fn make_row(row: [c_int; 4]) -> TricksRow {
            #[allow(clippy::cast_sign_loss)]
            TricksRow::new(
                (row[0] & 0xFF) as u8,
                (row[1] & 0xFF) as u8,
                (row[2] & 0xFF) as u8,
                (row[3] & 0xFF) as u8,
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

impl From<TricksTable> for sys::ddTableResults {
    fn from(table: TricksTable) -> Self {
        const fn make_row(row: TricksRow) -> [c_int; 4] {
            [
                row.get(Seat::North) as c_int,
                row.get(Seat::East) as c_int,
                row.get(Seat::South) as c_int,
                row.get(Seat::West) as c_int,
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

impl From<Deal> for sys::ddTableDeal {
    fn from(deal: Deal) -> Self {
        Self {
            cards: Seat::ALL.map(|seat| {
                let hand = deal[seat];
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

bitflags::bitflags! {
    /// Vulnerability of pairs
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    pub struct Vulnerability: u8 {
        /// North-South are vulnerable
        const NS = 1;
        /// East-West are vulnerable
        const EW = 2;
    }
}

impl Vulnerability {
    /// Both sides vulnerable
    pub const ALL: Self = Self::all();

    /// Neither side vulnerable
    pub const NONE: Self = Self::empty();

    /// Convert to encoding in [`dds_bridge_sys`]
    #[must_use]
    #[inline]
    pub const fn to_sys(self) -> i32 {
        const ALL: u8 = Vulnerability::all().bits();
        const NS: u8 = Vulnerability::NS.bits();
        const EW: u8 = Vulnerability::EW.bits();

        match self.bits() {
            0 => 0,
            ALL => 1,
            NS => 2,
            EW => 3,
            _ => unreachable!(),
        }
    }

    /// Conditionally swap [`NS`](Self::NS) and [`EW`](Self::EW)
    #[must_use]
    #[inline]
    pub const fn rotate(self, condition: bool) -> Self {
        // The trick: multiplying by 0x55 duplicates bit 0 into bit 1 (and bit 1
        // into bit 2, etc.).  Shifting right by 1 when `condition` is true
        // effectively moves bit 0 → nowhere and bit 1 → bit 0, swapping NS/EW.
        Self::from_bits_truncate((self.bits() * 0x55) >> (condition as u8))
    }
}

/// Error returned when parsing a [`Vulnerability`] fails
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("Invalid vulnerability: expected one of none, ns, ew, both")]
pub struct ParseVulnerabilityError;

impl FromStr for Vulnerability {
    type Err = ParseVulnerabilityError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_str() {
            "none" => Ok(Self::NONE),
            "ns" => Ok(Self::NS),
            "ew" => Ok(Self::EW),
            "both" | "all" => Ok(Self::ALL),
            _ => Err(ParseVulnerabilityError),
        }
    }
}

impl fmt::Display for Vulnerability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            Self::NONE => "none",
            Self::NS => "ns",
            Self::EW => "ew",
            Self::ALL => "both",
            _ => unreachable!(),
        })
    }
}

/// Exhaustively check correctness of [`Vulnerability::rotate`] at compile time
const _: () = {
    const ALL: Vulnerability = Vulnerability::all();
    const NONE: Vulnerability = Vulnerability::empty();

    assert!(matches!(ALL.rotate(true), ALL));
    assert!(matches!(NONE.rotate(true), NONE));
    assert!(matches!(Vulnerability::NS.rotate(true), Vulnerability::EW));
    assert!(matches!(Vulnerability::EW.rotate(true), Vulnerability::NS));

    assert!(matches!(ALL.rotate(false), ALL));
    assert!(matches!(NONE.rotate(false), NONE));
    assert!(matches!(Vulnerability::NS.rotate(false), Vulnerability::NS));
    assert!(matches!(Vulnerability::EW.rotate(false), Vulnerability::EW));
};

/// Par contract
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ParContract {
    /// The contract
    pub contract: Contract,

    /// The declarer of the contract
    pub declarer: Seat,

    /// The number of overtricks (negative for undertricks)
    pub overtricks: i8,
}

/// Par score and contracts
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Par {
    /// The par score
    pub score: i32,

    /// The contracts that achieve the par score
    pub contracts: Vec<ParContract>,
}

impl Par {
    /// Check if two pars are equivalent
    ///
    /// Two pars are equivalent if they have the same par score and the same
    /// set of (strain, declarer) pairs.  Overtricks and duplicate entries are
    /// ignored.
    ///
    /// This is intentionally looser than [`PartialEq`], which compares every
    /// field exactly.  `equivalent` exists because DDS may report the same
    /// par result with different overtrick counts or orderings depending on
    /// the code path (e.g. `DealerParBin` vs `SidesParBin`).  Use `==` when
    /// you need exact structural equality; use `equivalent` when you only
    /// care about the strategic meaning of the par result.
    #[must_use]
    pub fn equivalent(&self, other: &Self) -> bool {
        // Since every contract scores the same, we can compare only the set of
        // (`Strain`, `Seat`).  #`Strain` * #`Seat` = 5 * 4 = 20, which fits
        // in a `u32` as a bitset.
        fn key(contracts: &[ParContract]) -> u32 {
            contracts
                .iter()
                .map(|p| 1 << ((p.contract.bid.strain as u8) << 2 | p.declarer as u8))
                .fold(0, u32::bitor)
        }
        self.score == other.score && key(&self.contracts) == key(&other.contracts)
    }
}

impl From<sys::parResultsMaster> for Par {
    fn from(par: sys::parResultsMaster) -> Self {
        // DDS returns a zero contract for par-zero deals, but we want to filter
        // it out for consistency.
        #[allow(clippy::cast_sign_loss)]
        let len = par.number as usize * usize::from(par.contracts[0].level != 0);

        #[allow(clippy::cast_sign_loss)]
        let contracts = par.contracts[..len]
            .iter()
            .flat_map(|contract| {
                let strain = [
                    Strain::Notrump,
                    Strain::Spades,
                    Strain::Hearts,
                    Strain::Diamonds,
                    Strain::Clubs,
                ][contract.denom as usize];

                let (penalty, overtricks) = if contract.underTricks > 0 {
                    debug_assert!(contract.underTricks <= 13);
                    (Penalty::Doubled, -((contract.underTricks & 0xFF) as i8))
                } else {
                    debug_assert!(contract.overTricks >= 0 && contract.overTricks <= 13);
                    (Penalty::Undoubled, (contract.overTricks & 0xFF) as i8)
                };

                debug_assert_eq!(contract.level, contract.level & 7);
                let seat = match contract.seats & 3 {
                    0 => Seat::North,
                    1 => Seat::East,
                    2 => Seat::South,
                    _ => Seat::West,
                };
                let is_pair = contract.seats >= 4;
                #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
                let contract = Contract::new(contract.level as u8, strain, penalty);

                core::iter::once(ParContract {
                    contract,
                    declarer: seat,
                    overtricks,
                })
                .chain(if is_pair {
                    Some(ParContract {
                        contract,
                        declarer: seat.partner(),
                        overtricks,
                    })
                } else {
                    None
                })
            })
            .collect();

        Self {
            score: par.score,
            contracts,
        }
    }
}

/// Calculate par score and contracts for a deal
///
/// - `tricks`: The number of tricks each seat can take as declarer for each strain
/// - `vul`: The vulnerability of pairs
/// - `dealer`: The dealer of the deal
///
/// # Panics
///
/// Panics if DDS returns an error status.
#[must_use]
pub fn calculate_par(tricks: TricksTable, vul: Vulnerability, dealer: Seat) -> Par {
    let mut par = sys::parResultsMaster::default();
    let status = unsafe {
        sys::DealerParBin(
            &mut tricks.into(),
            &raw mut par,
            vul.to_sys(),
            dealer as c_int,
        )
    };
    check(status);
    par.into()
}

/// Calculate par scores for both pairs
///
/// - `tricks`: The number of tricks each seat can take as declarer for each strain
/// - `vul`: The vulnerability of pairs
///
/// # Panics
///
/// Panics if DDS returns an error status.
#[must_use]
pub fn calculate_pars(tricks: TricksTable, vul: Vulnerability) -> [Par; 2] {
    let mut pars = [sys::parResultsMaster::default(); 2];
    // SAFE: calculating par is reentrant
    let status = unsafe { sys::SidesParBin(&mut tricks.into(), &raw mut pars[0], vul.to_sys()) };
    check(status);
    pars.map(Into::into)
}

/// Target tricks and number of solutions to find
///
/// This enum corresponds to a tuple of `target` and `solutions` in
/// [`sys::SolveBoard`].  The `target` tricks given as an associated value must
/// be in the range of `-1..=13`, where `-1` instructs the solver to find cards
/// that give the most tricks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    #[inline]
    pub const fn target(self) -> c_int {
        match self {
            Self::Any(target) | Self::All(target) => target as c_int,
            Self::Legal => -1,
        }
    }

    /// Get the `solutions` argument for [`sys::SolveBoard`]
    #[must_use]
    #[inline]
    pub const fn solutions(self) -> c_int {
        match self {
            Self::Any(_) => 1,
            Self::All(_) => 2,
            Self::Legal => 3,
        }
    }
}

/// A snapshot of a board
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Board {
    /// The strain of the contract
    pub trump: Strain,
    /// The player leading the trick
    pub lead: Seat,
    /// The played cards in the current trick
    pub current_cards: ArrayVec<Card, 3>,
    /// The remaining cards in the deal
    pub remaining: Deal,
}

impl From<Board> for sys::deal {
    fn from(board: Board) -> Self {
        let mut suits = [0; 3];
        let mut ranks = [0; 3];

        for (i, card) in board.current_cards.into_iter().enumerate() {
            suits[i] = 3 - card.suit as c_int;
            ranks[i] = c_int::from(card.rank.get());
        }

        Self {
            trump: match board.trump {
                Strain::Spades => 0,
                Strain::Hearts => 1,
                Strain::Diamonds => 2,
                Strain::Clubs => 3,
                Strain::Notrump => 4,
            },
            first: board.lead as c_int,
            currentTrickSuit: suits,
            currentTrickRank: ranks,
            remainCards: sys::ddTableDeal::from(board.remaining).cards,
        }
    }
}

/// A board and its solving target
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Objective {
    /// The board to solve
    pub board: Board,
    /// The target tricks and number of solutions to find
    pub target: Target,
}

/// A starting board and a sequence of cards played from it
///
/// Input to [`Solver::analyse_play`].  The two fields split the position and
/// the play-trace cleanly:
///
/// - [`board`](Self::board) is the snapshot from which analysis begins.  It
///   encodes the state at the start of a trick — possibly with up to three
///   cards already on the table in [`Board::current_cards`] — and
///   [`Board::remaining`] holds only the cards still in each hand.  Cards
///   from **previously completed tricks are not represented individually**;
///   they are simply absent from `remaining`.
/// - [`cards`](Self::cards) is the play trace to replay from that snapshot,
///   in chronological order.  The first card in `cards` is whichever card
///   comes *after* any already in `board.current_cards` — it does **not**
///   restart the trick or repeat prior history.  Each card must be legal
///   (follow suit when possible and be held by the player on turn).
///
/// `cards` may span trick boundaries; DDS tracks trick completion and whose
/// lead follows internally.  The trace length may be any value from `0` to
/// `52`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PlayTrace {
    /// Snapshot at the start of analysis: state at the start of the current
    /// trick, plus any 0–3 cards already played to it via
    /// [`Board::current_cards`]
    pub board: Board,
    /// Cards played after `board`, in chronological order; may cross tricks
    pub cards: ArrayVec<Card, 52>,
}

impl From<&[Card]> for PlayTraceBin {
    fn from(cards: &[Card]) -> Self {
        let mut play = sys::playTraceBin::default();
        #[allow(clippy::cast_possible_wrap, clippy::cast_possible_truncation)]
        {
            play.number = cards.len() as c_int;
        }
        for (i, card) in cards.iter().enumerate() {
            play.suit[i] = 3 - card.suit as c_int;
            play.rank[i] = c_int::from(card.rank.get());
        }
        Self(play)
    }
}

/// Thin wrapper over [`sys::playTraceBin`] so we can impl `From<&[Card]>` for it
#[repr(transparent)]
struct PlayTraceBin(sys::playTraceBin);

/// Double-dummy trick counts before and after each played card in a trace
///
/// Returned by [`Solver::analyse_play`].  Trick counts are from the declarer's
/// viewpoint: declarer is the right-hand opponent of the opening leader (the
/// side to lead the very first trick in the starting [`Board`]).
///
/// `tricks[0]` is the DD value before any card in the trace is played.
/// `tricks[i]` for `i > 0` is the DD value after the i-th card.  A drop from
/// `tricks[i - 1]` to `tricks[i]` means that card was a double-dummy mistake
/// by the side to move at the time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlayAnalysis {
    /// Trick counts — `cards.len() + 1` entries, starting with the position
    /// before any card is played
    pub tricks: ArrayVec<u8, 53>,
}

impl From<sys::solvedPlay> for PlayAnalysis {
    #[allow(clippy::cast_sign_loss)]
    fn from(solved: sys::solvedPlay) -> Self {
        let mut tricks = ArrayVec::new();
        for i in 0..solved.number as usize {
            #[allow(clippy::cast_possible_truncation)]
            tricks.push((solved.tricks[i] & 0xFF) as u8);
        }
        Self { tricks }
    }
}

/// A play and its consequences
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Play {
    /// The card to play, the highest in a sequence
    ///
    /// For example, if the solution is to play a card from ♥KQJ, this field
    /// would be ♥K.
    pub card: Card,

    /// Lower equals in the sequence
    ///
    /// Playing any card in a sequence is equal in bridge and many trick-taking
    /// games.  This field contains lower cards in the sequence as `card`.  For
    /// example, if the solution is to play KQJ, this field would contain QJ.
    pub equals: Holding,

    /// Tricks this play would score
    pub score: i8,
}

/// Solved plays for a board
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FoundPlays {
    /// The plays and their consequences
    pub plays: ArrayVec<Play, 13>,
    /// The number of nodes searched by the solver
    pub nodes: u32,
}

impl From<sys::futureTricks> for FoundPlays {
    #[allow(clippy::cast_sign_loss)]
    fn from(future: sys::futureTricks) -> Self {
        let mut plays = ArrayVec::new();

        for i in 0..future.cards as usize {
            let equals = Holding::from_bits_truncate((future.equals[i] & 0xFFFF) as u16);
            let score = (future.score[i] & 0xFF) as i8;

            plays.push(Play {
                card: Card {
                    suit: Suit::DESC[future.suit[i] as usize],
                    rank: Rank::new((future.rank[i] & 0xFF) as u8),
                },
                equals,
                score,
            });
        }

        Self {
            plays,
            nodes: future.nodes as u32,
        }
    }
}

/// OS platform reported by the DDS library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Platform {
    /// Microsoft Windows
    Windows,
    /// Cygwin (Windows POSIX layer)
    Cygwin,
    /// Linux
    Linux,
    /// Apple (macOS / iOS)
    Apple,
    /// Unknown or unrecognized platform
    Unknown(i32),
}

/// C++ compiler used to build the DDS library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Compiler {
    /// Microsoft Visual C++
    MSVC,
    /// MinGW
    MinGW,
    /// GNU g++
    GCC,
    /// Clang
    Clang,
    /// Unknown or unrecognized compiler
    Unknown(i32),
}

/// Threading model used by the DDS library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Threading {
    /// No threading
    None,
    /// Windows native threads
    Windows,
    /// OpenMP
    OpenMP,
    /// Grand Central Dispatch (Apple)
    GCD,
    /// Boost.Thread
    Boost,
    /// C++ standard library threads (`std::thread`)
    STL,
    /// Intel Threading Building Blocks
    TBB,
    /// Unknown or experimental threading model
    Unknown(i32),
}

/// Information about the DDS library and how it was built
///
/// Returned by [`Solver::system_info`].  Exposes the version, hardware
/// configuration (cores, threads, pointer width), and compile-time choices
/// (OS, compiler, threading model) that DDS was built with.
#[derive(Debug, Clone, Copy)]
pub struct SystemInfo(sys::DDSInfo);

impl SystemInfo {
    /// DDS version
    #[must_use]
    pub const fn version(&self) -> Version {
        #[allow(clippy::cast_sign_loss)]
        Version::new(
            self.0.major as u64,
            self.0.minor as u64,
            self.0.patch as u64,
        )
    }

    /// OS platform DDS was built for
    #[must_use]
    pub const fn platform(&self) -> Platform {
        match self.0.system {
            1 => Platform::Windows,
            2 => Platform::Cygwin,
            3 => Platform::Linux,
            4 => Platform::Apple,
            n => Platform::Unknown(n),
        }
    }

    /// Pointer size in bits (32 or 64)
    #[must_use]
    pub const fn num_bits(&self) -> u32 {
        #[allow(clippy::cast_sign_loss)]
        return self.0.numBits as u32;
    }

    /// C++ compiler DDS was built with
    #[must_use]
    pub const fn compiler(&self) -> Compiler {
        match self.0.compiler {
            1 => Compiler::MSVC,
            2 => Compiler::MinGW,
            3 => Compiler::GCC,
            4 => Compiler::Clang,
            n => Compiler::Unknown(n),
        }
    }

    /// Threading model DDS was built with
    ///
    /// Currently, [`dds_bridge_sys`] only supports [`Threading::STL`] for
    /// maximum compatibility, minimum code size, and competitive performance.
    /// Other variants may be activated in the future for specialized use cases
    /// or platforms.
    #[must_use]
    pub const fn threading(&self) -> Threading {
        match self.0.threading {
            0 => Threading::None,
            1 => Threading::Windows,
            2 => Threading::OpenMP,
            3 => Threading::GCD,
            4 => Threading::Boost,
            5 => Threading::STL,
            6 => Threading::TBB,
            n => Threading::Unknown(n),
        }
    }

    /// Number of CPU cores detected by DDS
    #[must_use]
    pub const fn num_cores(&self) -> usize {
        #[allow(clippy::cast_sign_loss)]
        return self.0.numCores as usize;
    }

    /// Number of threads configured in the DDS thread pool
    #[must_use]
    pub const fn num_threads(&self) -> usize {
        #[allow(clippy::cast_sign_loss)]
        return self.0.noOfThreads as usize;
    }

    /// Memory-size description for each thread slot
    ///
    /// A string such as `"0 S, 16 L"` where `L` denotes a large transposition
    /// table and `S` a small one.
    #[must_use]
    pub const fn thread_sizes(&self) -> &str {
        // SAFETY: DDS fills `threadSizes` with a null-terminated ASCII string.
        unsafe { c_chars_to_str(&self.0.threadSizes) }
    }

    /// Human-readable summary of the full DDS system configuration
    #[must_use]
    pub const fn system_string(&self) -> &str {
        // SAFETY: DDS fills `systemString` with a null-terminated ASCII string.
        unsafe { c_chars_to_str(&self.0.systemString) }
    }
}

const unsafe fn c_chars_to_str(bytes: &[c_char]) -> &str {
    unsafe { core::str::from_utf8_unchecked(CStr::from_ptr(bytes.as_ptr()).to_bytes()) }
}

impl fmt::Display for SystemInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.system_string())
    }
}

static THREAD_POOL: LazyLock<Mutex<()>> = LazyLock::new(|| {
    unsafe { sys::SetMaxThreads(0) };
    Mutex::new(())
});

/// Exclusive handle to the DDS solver
///
/// DDS functions are not reentrant, so this struct holds a lock on the global
/// thread pool.  Acquire a `Solver` once and call methods on it to avoid
/// repeated locking.
///
/// The batch functions ([`CalcAllTables`](sys::CalcAllTables),
/// [`SolveAllBoardsBin`](sys::SolveAllBoardsBin)) are internally
/// multi-threaded, so parallelism is still utilized within each call.
pub struct Solver(#[allow(dead_code)] parking_lot::MutexGuard<'static, ()>);

impl Solver {
    /// Acquire exclusive access to the DDS solver, blocking until available
    #[must_use]
    pub fn lock() -> Self {
        Self(THREAD_POOL.lock())
    }

    /// Try to acquire exclusive access to the DDS solver without blocking
    ///
    /// Returns `None` if the solver is currently in use.
    #[must_use]
    pub fn try_lock() -> Option<Self> {
        THREAD_POOL.try_lock().map(Self)
    }

    /// Get information about the underlying DDS library
    #[must_use]
    pub fn system_info(&self) -> SystemInfo {
        let mut inner = MaybeUninit::uninit();
        unsafe { sys::GetDDSInfo(inner.as_mut_ptr()) };
        SystemInfo(unsafe { inner.assume_init() })
    }

    /// Solve a single deal with [`sys::CalcDDtable`]
    ///
    /// # Panics
    ///
    /// Panics if DDS returns an error status.
    ///
    /// # Examples
    ///
    /// ```
    /// use dds_bridge::{Deal, Seat, Solver, Strain};
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// // Each player holds a 13-card straight flush in one suit.
    /// let deal: Deal = "N:AKQJT98765432... .AKQJT98765432.. \
    ///                   ..AKQJT98765432. ...AKQJT98765432".parse()?;
    /// let tricks = Solver::lock().solve_deal(deal);
    /// // North holds all the spades, so North or South declaring spades
    /// // draws trumps and takes every trick.
    /// assert_eq!(tricks[Strain::Spades].get(Seat::North), 13);
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn solve_deal(&self, deal: Deal) -> TricksTable {
        let mut result = sys::ddTableResults::default();
        let status = unsafe { sys::CalcDDtable(deal.into(), &raw mut result) };
        check(status);
        result.into()
    }

    /// Solve deals with a single call of [`sys::CalcAllTables`]
    ///
    /// - `deals`: A slice of deals to solve
    /// - `flags`: Flags of strains to solve for
    ///
    /// # Safety
    ///
    /// 1. **Thread-unsafe:** The caller must ensure that no other thread is
    ///    calling any DDS function while this function is running.  This is
    ///    automatically guaranteed if the caller acquires a `Solver` before
    ///    calling this function.
    /// 2. `deals.len() * flags.bits().count_ones()` must not exceed
    ///    [`sys::MAXNOOFBOARDS`].
    ///
    unsafe fn solve_deal_segment(deals: &[Deal], flags: StrainFlags) -> sys::ddTablesRes {
        debug_assert!(
            deals.len() * flags.bits().count_ones() as usize <= sys::MAXNOOFBOARDS as usize
        );
        let mut pack = sys::ddTableDeals {
            noOfTables: c_int::try_from(deals.len()).unwrap_or(c_int::MAX),
            ..Default::default()
        };
        deals
            .iter()
            .enumerate()
            .for_each(|(i, &deal)| pack.deals[i] = deal.into());

        let mut filter = [
            c_int::from(!flags.contains(StrainFlags::SPADES)),
            c_int::from(!flags.contains(StrainFlags::HEARTS)),
            c_int::from(!flags.contains(StrainFlags::DIAMONDS)),
            c_int::from(!flags.contains(StrainFlags::CLUBS)),
            c_int::from(!flags.contains(StrainFlags::NOTRUMP)),
        ];
        let mut res = sys::ddTablesRes::default();
        let status = unsafe {
            sys::CalcAllTables(
                &raw mut pack,
                -1,
                filter.as_mut_ptr(),
                &raw mut res,
                &mut sys::allParResults::default(),
            )
        };
        check(status);
        res
    }

    /// Solve deals in parallel for given strains
    ///
    /// - `deals`: A slice of deals to solve
    /// - `flags`: Flags of strains to solve for
    ///
    /// # Panics
    ///
    /// - Panics if `flags` is empty.
    /// - Panics if DDS returns an error status.
    #[must_use]
    pub fn solve_deals(&self, deals: &[Deal], flags: StrainFlags) -> Vec<TricksTable> {
        let mut tables = Vec::new();
        for chunk in deals.chunks((sys::MAXNOOFBOARDS / flags.bits().count_ones()) as usize) {
            tables.extend(
                unsafe { Self::solve_deal_segment(chunk, flags) }.results[..chunk.len()]
                    .iter()
                    .map(|&x| TricksTable::from(x)),
            );
        }
        tables
    }

    /// Solve a single board with [`sys::SolveBoard`]
    ///
    /// # Panics
    ///
    /// Panics if DDS returns an error status.
    #[must_use]
    pub fn solve_board(&self, objective: Objective) -> FoundPlays {
        let mut result = sys::futureTricks::default();
        let status = unsafe {
            sys::SolveBoard(
                objective.board.into(),
                objective.target.target(),
                objective.target.solutions(),
                0,
                &raw mut result,
                0,
            )
        };
        check(status);
        FoundPlays::from(result)
    }

    /// Solve boards with a single call of [`sys::SolveAllBoardsBin`]
    ///
    /// - `args`: A slice of objectives to solve
    ///
    /// # Safety
    ///
    /// 1. **Thread-unsafe:** The caller must ensure that no other thread is
    ///    calling any DDS function while this function is running.  This is
    ///    automatically guaranteed if the caller acquires a `Solver` before
    ///    calling this function.
    /// 2. `args.len()` must not exceed [`sys::MAXNOOFBOARDS`].
    ///
    unsafe fn solve_board_segment(args: &[Objective]) -> sys::solvedBoards {
        debug_assert!(args.len() <= sys::MAXNOOFBOARDS as usize);
        let mut pack = sys::boards {
            #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
            noOfBoards: args.len() as c_int,
            ..Default::default()
        };
        args.iter().enumerate().for_each(|(i, obj)| {
            pack.deals[i] = obj.board.clone().into();
            pack.target[i] = obj.target.target();
            pack.solutions[i] = obj.target.solutions();
        });
        let mut res = sys::solvedBoards::default();
        let status = unsafe { sys::SolveAllBoardsBin(&raw mut pack, &raw mut res) };
        check(status);
        res
    }

    /// Solve boards in parallel
    ///
    /// - `args`: A slice of boards and their targets to solve
    ///
    /// # Panics
    ///
    /// Panics if DDS returns an error status.
    #[must_use]
    pub fn solve_boards(&self, args: &[Objective]) -> Vec<FoundPlays> {
        let mut solutions = Vec::new();
        for chunk in args.chunks(sys::MAXNOOFBOARDS as usize) {
            solutions.extend(
                unsafe { Self::solve_board_segment(chunk) }.solvedBoard[..chunk.len()]
                    .iter()
                    .map(|&x| FoundPlays::from(x)),
            );
        }
        solutions
    }

    /// Trace DD trick counts before and after each played card with
    /// [`sys::AnalysePlayBin`]
    ///
    /// # Panics
    ///
    /// Panics if DDS returns an error status, which includes the trace
    /// containing an invalid card (not held by the player on turn) or a
    /// disallowed revoke.
    #[must_use]
    pub fn analyse_play(&self, trace: PlayTrace) -> PlayAnalysis {
        let mut result = sys::solvedPlay::default();
        let play: PlayTraceBin = trace.cards.as_slice().into();
        let status = unsafe { sys::AnalysePlayBin(trace.board.into(), play.0, &raw mut result, 0) };
        check(status);
        PlayAnalysis::from(result)
    }

    /// Analyse play traces with a single call of [`sys::AnalyseAllPlaysBin`]
    ///
    /// # Safety
    ///
    /// 1. **Thread-unsafe:** The caller must ensure that no other thread is
    ///    calling any DDS function while this function is running.  This is
    ///    automatically guaranteed if the caller acquires a `Solver` before
    ///    calling this function.
    /// 2. `traces.len()` must not exceed [`sys::MAXNOOFBOARDS`].
    ///
    unsafe fn analyse_play_segment(traces: &[PlayTrace]) -> sys::solvedPlays {
        debug_assert!(traces.len() <= sys::MAXNOOFBOARDS as usize);
        let mut pack = sys::boards {
            #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
            noOfBoards: traces.len() as c_int,
            ..Default::default()
        };
        let mut plays = sys::playTracesBin {
            #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
            noOfBoards: traces.len() as c_int,
            ..Default::default()
        };
        traces.iter().enumerate().for_each(|(i, trace)| {
            pack.deals[i] = trace.board.clone().into();
            plays.plays[i] = PlayTraceBin::from(trace.cards.as_slice()).0;
        });
        let mut res = sys::solvedPlays::default();
        let status =
            unsafe { sys::AnalyseAllPlaysBin(&raw mut pack, &raw mut plays, &raw mut res, 0) };
        check(status);
        res
    }

    /// Trace DD trick counts in parallel with [`sys::AnalyseAllPlaysBin`]
    ///
    /// # Panics
    ///
    /// Panics if DDS returns an error status, which includes any trace
    /// containing an invalid card (not held by the player on turn) or a
    /// disallowed revoke.
    #[must_use]
    pub fn analyse_plays(&self, traces: &[PlayTrace]) -> Vec<PlayAnalysis> {
        let mut results = Vec::new();
        for chunk in traces.chunks(sys::MAXNOOFBOARDS as usize) {
            results.extend(
                unsafe { Self::analyse_play_segment(chunk) }.solved[..chunk.len()]
                    .iter()
                    .map(|&x| PlayAnalysis::from(x)),
            );
        }
        results
    }
}
