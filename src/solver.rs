//! Double-dummy solver and par-calculation bindings built on [`dds_bridge_sys`].
//!
//! # Panic policy
//!
//! The solver entry points in this module — [`calculate_par`],
//! [`calculate_pars`], and the [`Solver`] methods
//! [`solve_deal`](Solver::solve_deal), [`solve_deals`](Solver::solve_deals),
//! [`solve_board`](Solver::solve_board), [`solve_boards`](Solver::solve_boards),
//! [`analyse_play`](Solver::analyse_play), and
//! [`analyse_plays`](Solver::analyse_plays) — are not expected to panic.
//! They map DDS status codes through an internal helper that panics on error,
//! but reaching that panic means either invalid input slipped past a safe
//! constructor or DDS itself misbehaved. Either case is a bug — please report
//! it.
//!
//! This policy does not cover validator panics from safe constructors
//! (e.g. [`TrickCountRow::new`](crate::solver::TrickCountRow::new)), which
//! panic by design on out-of-range inputs and have `try_*` counterparts for
//! fallible construction.

mod board;
mod ffi;
mod par;
mod play;
mod strain_flags;
mod system_info;
mod tricks;
mod vulnerability;

pub use board::*;
pub use par::*;
pub use play::*;
pub use strain_flags::*;
pub use system_info::*;
pub use tricks::*;
pub use vulnerability::*;

use crate::deal::FullDeal;
use crate::seat::Seat;

use dds_bridge_sys as sys;
use parking_lot::Mutex;

use core::ffi::c_int;
use core::mem::MaybeUninit;
use std::sync::LazyLock;

/// Maximum number of boards that can be solved in a single batch call to DDS
///
/// This is a hard limit in DDS, not a limit of this crate.  The batch methods
/// in [`Solver`] will automatically split their input into segments of this
/// size or smaller, so users of this crate don't need to worry about it as long
/// as they use the batch methods for large inputs.  However, if users call the
/// unsafe segment methods directly, they must ensure that their input sizes
/// don't exceed this limit.
///
/// See also [`sys::MAXNOOFBOARDS`] and the safety requirements of the batch
/// methods in [`Solver`].
const MAX_BOARD_COUNT: usize = sys::MAXNOOFBOARDS as usize;

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

/// Calculate par score and contracts for a deal
///
/// - `tricks`: The number of tricks each seat can take as declarer for each strain
/// - `vul`: The vulnerability of pairs
/// - `dealer`: The dealer of the deal
///
/// # Panics
///
/// Not expected — panics here are bugs. See the module-level panic policy.
#[must_use]
pub fn calculate_par(tricks: TrickCountTable, vul: Vulnerability, dealer: Seat) -> Par {
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
/// Not expected — panics here are bugs. See the module-level panic policy.
#[must_use]
pub fn calculate_pars(tricks: TrickCountTable, vul: Vulnerability) -> [Par; 2] {
    let mut pars = [sys::parResultsMaster::default(); 2];
    // SAFE: calculating par is reentrant
    let status = unsafe { sys::SidesParBin(&mut tricks.into(), &raw mut pars[0], vul.to_sys()) };
    check(status);
    pars.map(Into::into)
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
    /// Not expected — panics here are bugs. See the module-level panic policy.
    ///
    /// # Examples
    ///
    /// ```
    /// use dds_bridge::{FullDeal, Seat, Solver, Strain};
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// // Each player holds a 13-card straight flush in one suit.
    /// let deal: FullDeal = "N:AKQJT98765432... .AKQJT98765432.. \
    ///                       ..AKQJT98765432. ...AKQJT98765432".parse()?;
    /// let tricks = Solver::lock().solve_deal(deal);
    /// // North holds all the spades, so North or South declaring spades
    /// // draws trumps and takes every trick.
    /// assert_eq!(u8::from(tricks[Strain::Spades].get(Seat::North)), 13);
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn solve_deal(&self, deal: FullDeal) -> TrickCountTable {
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
    unsafe fn solve_deal_segment(
        deals: &[FullDeal],
        flags: NonEmptyStrainFlags,
    ) -> sys::ddTablesRes {
        let flags = flags.get();
        let strain_count = flags.bits().count_ones() as usize;

        let mut pack = sys::ddTableDeals {
            noOfTables: ffi::count_to_sys(deals.len(), MAX_BOARD_COUNT / strain_count),
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
    /// Not expected — panics here are bugs. See the module-level panic policy.
    #[must_use]
    pub fn solve_deals(
        &self,
        deals: &[FullDeal],
        flags: NonEmptyStrainFlags,
    ) -> Vec<TrickCountTable> {
        let mut tables = Vec::new();
        for chunk in deals.chunks((sys::MAXNOOFBOARDS / flags.get().bits().count_ones()) as usize) {
            tables.extend(
                unsafe { Self::solve_deal_segment(chunk, flags) }.results[..chunk.len()]
                    .iter()
                    .map(|&x| TrickCountTable::from(x)),
            );
        }
        tables
    }

    /// Solve a single board with [`sys::SolveBoard`]
    ///
    /// # Panics
    ///
    /// Not expected — panics here are bugs. See the module-level panic policy.
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
        let mut pack = sys::boards {
            noOfBoards: ffi::count_to_sys(args.len(), MAX_BOARD_COUNT),
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
    /// Not expected — panics here are bugs. See the module-level panic policy.
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
    /// Not expected — panics here are bugs. See the module-level panic policy.
    #[must_use]
    pub fn analyse_play(&self, trace: PlayTrace) -> PlayAnalysis {
        let mut result = sys::solvedPlay::default();
        let play = PlayTraceBin::from(&trace.cards);
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
        let mut pack = sys::boards {
            noOfBoards: ffi::count_to_sys(traces.len(), MAX_BOARD_COUNT),
            ..Default::default()
        };
        let mut plays = sys::playTracesBin {
            noOfBoards: ffi::count_to_sys(traces.len(), MAX_BOARD_COUNT),
            ..Default::default()
        };
        traces.iter().enumerate().for_each(|(i, trace)| {
            pack.deals[i] = trace.board.clone().into();
            plays.plays[i] = PlayTraceBin::from(&trace.cards).0;
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
    /// Not expected — panics here are bugs. See the module-level panic policy.
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
