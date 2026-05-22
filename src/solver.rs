//! Double-dummy solver and par-calculation bindings built on [`dds_bridge_sys`].
//!
//! # Panic policy
//!
//! The solver entry points in this module — [`calculate_par`],
//! [`calculate_pars`], [`Solver::solve_deal`], [`Solver::solve_board`],
//! [`solve_deals`], [`solve_boards`], [`analyse_play`], and [`analyse_plays`]
//! — are not expected to panic.  They map DDS status codes through an
//! internal helper that panics on error, but reaching that panic means
//! either invalid input slipped past a safe constructor or DDS itself
//! misbehaved. Either case is a bug — please report it.
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
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use core::ffi::c_int;
use core::mem::MaybeUninit;
use core::ptr::NonNull;
use std::sync::LazyLock;

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

/// Kind of transposition table to allocate inside a [`Solver`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TtKind {
    /// Small TT — lower memory footprint
    Small,
    /// Large TT — higher memory footprint, faster on bigger search trees
    Large,
}

impl TtKind {
    #[allow(clippy::cast_possible_wrap)]
    const fn to_sys(self) -> c_int {
        match self {
            Self::Small => sys::DDS_TT_KIND_SMALL as c_int,
            Self::Large => sys::DDS_TT_KIND_LARGE as c_int,
        }
    }
}

/// Configuration for a [`Solver`]
///
/// `0` for either memory field means "use the upstream default".
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SolverConfig {
    /// Kind of transposition table to allocate
    pub tt_kind: TtKind,
    /// Default (initial) TT size in MiB; `0` for the upstream default
    pub tt_mem_default_mb: u32,
    /// Maximum TT size in MiB; `0` for the upstream default
    pub tt_mem_maximum_mb: u32,
}

impl Default for SolverConfig {
    fn default() -> Self {
        Self {
            tt_kind: TtKind::Large,
            tt_mem_default_mb: 0,
            tt_mem_maximum_mb: 0,
        }
    }
}

impl SolverConfig {
    fn to_sys(self) -> sys::DdsSolverConfig {
        sys::DdsSolverConfig {
            tt_kind: self.tt_kind.to_sys(),
            tt_mem_default_mb: c_int::try_from(self.tt_mem_default_mb)
                .expect("tt_mem_default_mb fits in c_int"),
            tt_mem_maximum_mb: c_int::try_from(self.tt_mem_maximum_mb)
                .expect("tt_mem_maximum_mb fits in c_int"),
        }
    }
}

/// Owned handle to a DDS solver context
///
/// One `Solver` owns one DDS `SolverContext` (private solver state:
/// thread-local memory, transposition table, search state) and is the
/// upstream-recommended way to drive DDS in parallel: one `Solver` per OS
/// thread, never shared.
///
/// The handle is [`Send`] (work-stealing pools may move it between threads
/// as long as no two threads access it at once) but not [`Sync`] — upstream
/// forbids concurrent access from multiple threads to a single context.
///
/// The transposition table is preserved across calls on the same `Solver`,
/// so reusing one over a batch of related queries amortizes setup cost.
/// For batches of unrelated queries the free helpers [`solve_deals`] and
/// [`solve_boards`] fan work across rayon workers with one `Solver` per
/// worker.
///
/// [`Drop`] calls `dds_solver_context_free`.
pub struct Solver {
    handle: NonNull<sys::DdsSolverContext>,
}

// SAFETY: ownership is single-threaded at any one time. Sending the handle
// across threads is fine; concurrent shared access is not (hence !Sync).
unsafe impl Send for Solver {}

impl Default for Solver {
    /// Construct a new solver with the [`SolverConfig::default`] configuration.
    fn default() -> Self {
        Self::new(SolverConfig::default())
    }
}

impl Solver {
    /// Construct a new solver with the given configuration
    ///
    /// # Panics
    ///
    /// If the C++ allocator returns a null pointer.
    #[must_use]
    pub fn new(config: SolverConfig) -> Self {
        let cfg = config.to_sys();
        // SAFETY: cfg is a valid, properly-initialized struct.
        let raw = unsafe { sys::dds_solver_context_new(&raw const cfg) };
        let handle = NonNull::new(raw).expect("dds_solver_context_new returned null");
        Self { handle }
    }

    /// Solve a single deal for all strains and all declarers
    ///
    /// Resets internal search state before solving so the result does not
    /// depend on previous solves on this `Solver` (the transposition table
    /// is preserved across calls).
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
    /// # fn main() -> Result<(), Box<dyn core::error::Error>> {
    /// // Each player holds a 13-card straight flush in one suit.
    /// let deal: FullDeal = "N:AKQJT98765432... .AKQJT98765432.. \
    ///                       ..AKQJT98765432. ...AKQJT98765432".parse()?;
    /// let mut solver = Solver::default();
    /// let tricks = solver.solve_deal(deal);
    /// // North holds all the spades, so North or South declaring spades
    /// // draws trumps and takes every trick.
    /// assert_eq!(u8::from(tricks[Strain::Spades].get(Seat::North)), 13);
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn solve_deal(&mut self, deal: FullDeal) -> TrickCountTable {
        let table_deal = sys::DdTableDeal::from(deal);
        let mut result = sys::DdTableResults::default();
        // SAFETY: handle is non-null and owned by self; pointers are valid
        // for the duration of the call.
        let status = unsafe {
            sys::dds_solver_context_reset_for_solve(self.handle.as_ptr());
            sys::dds_calc_dd_table(self.handle.as_ptr(), &raw const table_deal, &raw mut result)
        };
        check(status);
        result.into()
    }

    /// Solve a single board against an [`Objective`]
    ///
    /// Resets internal search state before solving so the result does not
    /// depend on previous solves on this `Solver` (the transposition table
    /// is preserved across calls).
    ///
    /// # Panics
    ///
    /// Not expected — panics here are bugs. See the module-level panic policy.
    #[must_use]
    pub fn solve_board(&mut self, objective: &Objective) -> FoundPlays {
        let deal = sys::Deal::from(objective.board.clone());
        let mut result = sys::FutureTricks::default();
        // SAFETY: handle is non-null and owned by self; pointers are valid
        // for the duration of the call.
        let status = unsafe {
            sys::dds_solver_context_reset_for_solve(self.handle.as_ptr());
            sys::dds_solve_board(
                self.handle.as_ptr(),
                &raw const deal,
                objective.target.target(),
                objective.target.solutions(),
                0,
                &raw mut result,
            )
        };
        check(status);
        FoundPlays::from(result)
    }
}

impl Drop for Solver {
    fn drop(&mut self) {
        // SAFETY: handle was returned by dds_solver_context_new and has not
        // been freed yet (Drop runs at most once).
        unsafe { sys::dds_solver_context_free(self.handle.as_ptr()) };
    }
}

/// Get information about the underlying DDS library
#[must_use]
pub fn system_info() -> SystemInfo {
    let mut inner = MaybeUninit::uninit();
    unsafe { sys::GetDDSInfo(inner.as_mut_ptr()) };
    SystemInfo(unsafe { inner.assume_init() })
}

/// Solve a slice of deals in parallel
///
/// Fans out across rayon workers; each worker owns one [`Solver`] and
/// reuses its transposition table across the deals it processes.
///
/// # Panics
///
/// Not expected — panics here are bugs. See the module-level panic policy.
#[must_use]
pub fn solve_deals(deals: &[FullDeal]) -> Vec<TrickCountTable> {
    deals
        .par_iter()
        .map_init(Solver::default, |s, &d| s.solve_deal(d))
        .collect()
}

/// Solve a slice of boards in parallel
///
/// Fans out across rayon workers; each worker owns one [`Solver`] and
/// reuses its transposition table across the boards it processes.
///
/// # Panics
///
/// Not expected — panics here are bugs. See the module-level panic policy.
#[must_use]
pub fn solve_boards(args: &[Objective]) -> Vec<FoundPlays> {
    args.par_iter()
        .map_init(Solver::default, |s, o| s.solve_board(o))
        .collect()
}

/// One-shot initialization of the legacy DDS thread pool, needed only by the
/// `AnalysePlayBin` path (the modern [`Solver`] context manages its own
/// threads).
static INIT_LEGACY_POOL: LazyLock<()> = LazyLock::new(|| unsafe { sys::SetMaxThreads(0) });

fn analyse_play_ref(trace: &PlayTrace) -> PlayAnalysis {
    LazyLock::force(&INIT_LEGACY_POOL);
    let mut result = sys::SolvedPlay::default();
    let play = PlayTraceBin::from(&trace.cards);
    let status =
        unsafe { sys::AnalysePlayBin(trace.board.clone().into(), play.0, &raw mut result, 0) };
    check(status);
    PlayAnalysis::from(result)
}

/// Trace DD trick counts before and after each played card with
/// [`sys::AnalysePlayBin`]
///
/// # Panics
///
/// Not expected — panics here are bugs. See the module-level panic policy.
#[must_use]
pub fn analyse_play(trace: PlayTrace) -> PlayAnalysis {
    analyse_play_ref(&trace)
}

/// Trace DD trick counts for many plays in parallel
///
/// Fans out across rayon workers, each calling [`sys::AnalysePlayBin`] with
/// `threadIndex = 0`.  Per the `dds-bridge-sys` documentation, this entry
/// point is safe for concurrent invocation across threads, though each call
/// pays the full setup cost of a fresh internal solver context (no TT
/// reuse across traces — the modern shim does not yet expose a context
/// variant of `analyse_play`).
///
/// # Panics
///
/// Not expected — panics here are bugs. See the module-level panic policy.
#[must_use]
pub fn analyse_plays(traces: &[PlayTrace]) -> Vec<PlayAnalysis> {
    traces.par_iter().map(analyse_play_ref).collect()
}
