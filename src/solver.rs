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
mod context;
mod ffi;
mod par;
mod play;
mod strain_flags;
mod system_info;
mod tricks;
mod vulnerability;

pub use board::*;
pub use context::*;
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
use rayon::iter::ParallelIterator;
use rayon::slice::ParallelSlice;

use core::ffi::c_int;
use core::mem::MaybeUninit;
use std::sync::LazyLock;

/// Worker-chunk size for rayon-driven batch methods.  Empirically a small
/// chunk (4–16) keeps load balanced across workers while amortizing per-task
/// overhead; the precise value is not API-visible.
const CHUNK_SIZE: usize = 8;

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
    let mut par = sys::ParResultsMaster::default();
    let status =
        unsafe { sys::DealerParBin(&tricks.into(), &raw mut par, vul.to_sys(), dealer as c_int) };
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
    let mut pars = [sys::ParResultsMaster::default(); 2];
    // SAFE: calculating par is reentrant
    let status = unsafe { sys::SidesParBin(&tricks.into(), &raw mut pars[0], vul.to_sys()) };
    check(status);
    pars.map(Into::into)
}

static THREAD_POOL: LazyLock<Mutex<()>> = LazyLock::new(|| {
    unsafe { sys::SetMaxThreads(0) };
    Mutex::new(())
});

/// Exclusive handle to the DDS solver
///
/// The legacy DDS C API was not reentrant, so this struct holds a global
/// mutex to keep the legacy entry points serialized.  Acquire a `Solver` once
/// and call methods on it to avoid repeated locking.
///
/// As of `dds-bridge-sys` 3.1 (DDS v3.0.0), batch methods
/// ([`solve_deals`](Self::solve_deals), [`solve_boards`](Self::solve_boards),
/// [`analyse_plays`](Self::analyse_plays)) parallelize across rayon workers
/// using one [`SolverContext`] per worker thread.  Single-deal methods stay
/// on the legacy thread-safe entry points.
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
        let mut result = sys::DdTableResults::default();
        let status = unsafe { sys::CalcDDtable(deal.into(), &raw mut result) };
        check(status);
        result.into()
    }

    /// Solve a slice of deals for given strains
    ///
    /// Drives a single [`SolverContext`] sequentially across all deals so the
    /// transposition table stays warm between solves — typically faster than
    /// looping over [`solve_deal`](Self::solve_deal), which throws away the
    /// transposition table after each call.
    ///
    /// Unlike [`solve_boards`](Self::solve_boards), this method does **not**
    /// parallelize across rayon workers: upstream DDS 3's `calc_dd_table`
    /// shares global scheduling buffers between contexts, so concurrent
    /// invocations corrupt each other.  Callers that need batch parallelism
    /// for DD tables should partition their input and call this method from
    /// multiple `Solver`s — but in practice [`solve_boards`](Self::solve_boards)
    /// (which has no such limitation) is the better building block.
    ///
    /// The `flags` argument is preserved for API compatibility but is
    /// informational: each solve returns the full 5×4 [`TrickCountTable`]
    /// regardless of strain filtering.
    ///
    /// # Panics
    ///
    /// Not expected — panics here are bugs. See the module-level panic policy.
    #[must_use]
    pub fn solve_deals(
        &self,
        deals: &[FullDeal],
        _flags: NonEmptyStrainFlags,
    ) -> Vec<TrickCountTable> {
        let mut ctx = SolverContext::default();
        deals.iter().map(|&deal| ctx.solve_deal(deal)).collect()
    }

    /// Solve a single board with [`sys::SolveBoard`]
    ///
    /// # Panics
    ///
    /// Not expected — panics here are bugs. See the module-level panic policy.
    #[must_use]
    pub fn solve_board(&self, objective: Objective) -> FoundPlays {
        let mut result = sys::FutureTricks::default();
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

    /// Solve boards in parallel
    ///
    /// Fans out across rayon workers; each worker owns one [`SolverContext`]
    /// and reuses its transposition table across the boards it processes.
    ///
    /// - `args`: A slice of boards and their targets to solve
    ///
    /// # Panics
    ///
    /// Not expected — panics here are bugs. See the module-level panic policy.
    #[must_use]
    pub fn solve_boards(&self, args: &[Objective]) -> Vec<FoundPlays> {
        let chunks: Vec<Vec<FoundPlays>> = args
            .par_chunks(CHUNK_SIZE)
            .map(|chunk| {
                let mut ctx = SolverContext::default();
                chunk
                    .iter()
                    .map(|obj| ctx.solve_board(obj.clone()))
                    .collect()
            })
            .collect();
        chunks.into_iter().flatten().collect()
    }

    /// Trace DD trick counts before and after each played card with
    /// [`sys::AnalysePlayBin`]
    ///
    /// # Panics
    ///
    /// Not expected — panics here are bugs. See the module-level panic policy.
    #[must_use]
    pub fn analyse_play(&self, trace: PlayTrace) -> PlayAnalysis {
        let mut result = sys::SolvedPlay::default();
        let play = PlayTraceBin::from(&trace.cards);
        let status = unsafe { sys::AnalysePlayBin(trace.board.into(), play.0, &raw mut result, 0) };
        check(status);
        PlayAnalysis::from(result)
    }

    /// Trace DD trick counts for many plays in parallel
    ///
    /// Fans out across rayon workers, each calling [`sys::AnalysePlayBin`]
    /// with `threadIndex = 0`.  Per the `dds-bridge-sys` documentation, this
    /// entry point is safe for concurrent invocation across threads, though
    /// each call pays the full setup cost of a fresh internal solver context
    /// (no TT reuse across traces — the modern shim does not yet expose a
    /// context variant of `analyse_play`).
    ///
    /// # Panics
    ///
    /// Not expected — panics here are bugs. See the module-level panic policy.
    #[must_use]
    pub fn analyse_plays(&self, traces: &[PlayTrace]) -> Vec<PlayAnalysis> {
        let chunks: Vec<Vec<PlayAnalysis>> = traces
            .par_chunks(CHUNK_SIZE)
            .map(|chunk| chunk.iter().map(analyse_play_single).collect())
            .collect();
        chunks.into_iter().flatten().collect()
    }
}

/// Single-trace play analysis used by [`Solver::analyse_plays`].
///
/// Calls [`sys::AnalysePlayBin`] with `threadIndex = 0`.  Safe to invoke
/// concurrently from multiple threads (each call constructs its own internal
/// solver context inside DDS).
fn analyse_play_single(trace: &PlayTrace) -> PlayAnalysis {
    let mut result = sys::SolvedPlay::default();
    let play = PlayTraceBin::from(&trace.cards);
    let status =
        unsafe { sys::AnalysePlayBin(trace.board.clone().into(), play.0, &raw mut result, 0) };
    check(status);
    PlayAnalysis::from(result)
}
