//! Safe wrapper for the DDS modern `SolverContext` C shim.
//!
//! Each [`SolverContext`] owns private solver state (thread-local memory,
//! transposition table, search state) and is the upstream-recommended way to
//! drive DDS in parallel: one context per OS thread, never shared.
//!
//! Used internally by [`Solver::solve_deals`](super::Solver::solve_deals),
//! [`Solver::solve_boards`](super::Solver::solve_boards), and
//! [`Solver::analyse_plays`](super::Solver::analyse_plays) to fan work out
//! across rayon workers. Also exposed publicly for callers that want
//! transposition-table reuse across related queries on a single thread.

use super::board::Objective;
use super::check;
use super::play::FoundPlays;
use super::tricks::TrickCountTable;
use crate::deal::FullDeal;

use dds_bridge_sys as sys;

use core::ffi::c_int;
use core::ptr::NonNull;

/// Kind of transposition table to allocate inside a [`SolverContext`]
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

/// Configuration for a [`SolverContext`]
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

/// Owned handle to a DDS `SolverContext`
///
/// One context per OS thread. The handle is `Send` (work-stealing pools may
/// move it between threads as long as no two threads access it at once) but
/// not `Sync` — upstream forbids concurrent access from multiple threads to a
/// single context.
///
/// Drop calls `dds_solver_context_free`.
pub struct SolverContext {
    handle: NonNull<sys::DdsSolverContext>,
}

// SAFETY: ownership is single-threaded at any one time. Sending the handle
// across threads is fine; concurrent shared access is not (hence !Sync).
unsafe impl Send for SolverContext {}

impl Default for SolverContext {
    /// Construct a new context with the [`SolverConfig::default`] configuration.
    fn default() -> Self {
        Self::new(SolverConfig::default())
    }
}

impl SolverContext {
    /// Construct a new context with the given configuration
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
    /// depend on previous solves on this context (the transposition table is
    /// preserved across calls).
    ///
    /// # Panics
    ///
    /// Not expected — panics here are bugs. See the module-level panic policy.
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
    /// depend on previous solves on this context (the transposition table is
    /// preserved across calls).
    ///
    /// # Panics
    ///
    /// Not expected — panics here are bugs. See the module-level panic policy.
    #[must_use]
    pub fn solve_board(&mut self, objective: Objective) -> FoundPlays {
        let deal = sys::Deal::from(objective.board);
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

impl Drop for SolverContext {
    fn drop(&mut self) {
        // SAFETY: handle was returned by dds_solver_context_new and has not
        // been freed yet (Drop runs at most once).
        unsafe { sys::dds_solver_context_free(self.handle.as_ptr()) };
    }
}
