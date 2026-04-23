# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- markdownlint-disable no-duplicate-heading -->

## [Unreleased]

### Internal

- Replaced non-const `.unwrap()` in tests with `?` propagation. Tests with a single fallible error type return `Result<(), E>`; tests mixing error types return `anyhow::Result<()>`. `anyhow` is added as a dev-dependency. Proptest strategy closures and the `subset_from` test helper use `.expect(…)` with invariant-describing messages instead of bare `.unwrap()`.

## [0.18.0] - 2026-04-24

The main idea of this release is to let the type system enforce solver preconditions.

### Changed

- **Breaking:** `BoardError::Revoke` now carries `position: RevokePosition` instead of `index: u8`. The new `RevokePosition` enum has variants `Second`, `Third`, and `Fourth`, encoding the compile-time-bounded set of positions where a revoke can occur (the lead cannot revoke). Update match arms from `BoardError::Revoke { index: 1 }` / `index: 2` to `BoardError::Revoke { position: RevokePosition::Second }` / `RevokePosition::Third`.
- **Breaking:** The trick-count types are renamed and gain a per-seat newtype:
  - `TricksRow` → `TrickCountRow`, `TricksTable` → `TrickCountTable` (and their `Hex` views).
  - New `TrickCount(u8)` newtype for a trick count in `0..=13`, analogous to `Level` / `Rank`. It provides `new`, `try_new`, `get() -> u8`, `Display`, and infallible `From<TrickCount>` for `u8` and `usize`.
  - `TrickCountRow::get(seat)` now returns `TrickCount` instead of `u8`. Call sites that index into arrays (e.g. histograms) keep working via `usize::from(row.get(seat))`; code that needs the raw byte can use `u8::from(...)` or `.get().get()`.
  - `Play::score` is `TrickCount` instead of `i8`, and `PlayAnalysis::tricks` is `ArrayVec<TrickCount, 53>` instead of `ArrayVec<u8, 53>`. Both fields are always in `0..=13`, so the new type matches the actual invariant. Widen to a numeric type with `u8::from(...)` / `usize::from(...)` at the use site.
  - The validation error `InvalidTricks` is renamed to `InvalidTrickCount` and is shared between `TrickCount::try_new` and `TrickCountRow::try_new`.
- **Breaking:** `TrickCountRow::new(n, e, s, w)` now panics (compile-time error in const contexts) when any value exceeds 13, instead of silently truncating to a 4-bit field. Use `TrickCountRow::try_new` to handle the error.
- **Breaking:** `Target::Any` and `Target::All` now carry `Option<TrickCount>` instead of `i8`. `Some(tc)` is "at least `tc` tricks"; `None` replaces the old `-1` sentinel for "find the most tricks". The FFI `-1` is still produced internally by `Target::target()` and is no longer part of the public payload, so out-of-range values like `Target::Any(-2)` or `Target::Any(42)` are no longer expressible. Migrate `Target::Any(-1)` / `Target::All(-1)` to `Target::Any(None)` / `Target::All(None)`, and `Target::Any(n)` / `Target::All(n)` (for `0 ≤ n ≤ 13`) to `Target::Any(Some(TrickCount::new(n as u8)))` / `Target::All(Some(TrickCount::new(n as u8)))`.
- **Breaking:** `Deal` is replaced by a type hierarchy with stricter invariants:
  - `Builder` — unvalidated `[Hand; 4]` with `IndexMut<Seat>`; the only mutable deal type. Direct successor of today's `Deal` for incremental construction.
  - `PartialDeal` — newtype over `Builder` with the invariant that each hand has ≤13 cards and the hands are pairwise disjoint. Read-only (`Index<Seat>` only). `FromStr` accepts PBN with partial holdings or `x` spots.
  - `FullDeal` — newtype over `Builder` with the invariant that each hand has exactly 13 cards. Read-only. `FromStr` is strict PBN.
  Convert between them via `Builder::build_partial` / `Builder::build_full`, the matching `TryFrom` impls (errors return the input unchanged), or infallible widenings (`From<FullDeal> for PartialDeal`, etc.). `PartialDeal::collected` replaces the old `Deal::validate_and_collect` as an infallible `Hand` accessor.
- **Breaking:** `Board` fields are now private. `Board::new` is removed; `Board::try_new(remaining, current_trick)` is the sole constructor, taking a pre-validated [`CurrentTrick`](#added) that owns `trump`, `lead`, and the 0–3 played cards. New accessors `trump()`, `lead()`, `current_cards()`, `current_trick()`, `remaining()`. The `remaining` field is now a `PartialDeal` rather than a `Deal`. New `BoardError` enum covers cross-cutting invariant violations (`PlayedCardInHand`, `InconsistentHandSizes`, `Revoke`); the trick-shape errors (`TooManyPlayed`, `DuplicatePlayedCard`) moved to `CurrentTrickError`.
- **Breaking:** `Solver::solve_deal` and `Solver::solve_deals` take `FullDeal` / `&[FullDeal]` instead of `Deal` / `&[Deal]`. The FFI converter `From<FullDeal> for sys::ddTableDeal` (plus `From<PartialDeal> for sys::ddTableDeal`) replaces `From<Deal>`.
- **Breaking:** `ParseDealError` gains two variants, `InvalidSubset` and `NotFullDeal`, emitted by the strict parsers.
- Panics from the solver entry points (`calculate_par`, `calculate_pars`, and `Solver::{solve_deal, solve_deals, solve_board, solve_boards, analyse_play, analyse_plays}`) are now considered bugs — please report them. These functions route DDS status codes through an internal helper that panics on error; reaching it means invalid input slipped past a safe constructor or DDS itself misbehaved. This policy does not cover validator panics from safe constructors (e.g. `TrickCountRow::new`), which panic by design on out-of-range inputs and have `try_*` counterparts for fallible construction.
- **Breaking:** `Seat`, `SeatFlags`, and `ParseSeatError` have moved out of the `deal` module into a new top-level `seat` module. The crate-root re-exports `dds_bridge::Seat` and `dds_bridge::SeatFlags` are unchanged, so most consumers need no code changes. Code that imported these types through the `deal` submodule path (e.g. `use dds_bridge::deal::Seat;`) must update to `dds_bridge::seat::Seat` or the crate-root re-export.
- The `solver` module is now a directory module: its contents are split across `strain_flags`, `tricks`, `vulnerability`, `par`, `board`, `play`, and `system_info` submodules. All items remain reachable at their original `dds_bridge::solver::*` paths through re-exports — no public API change.
- Dropped `debug_assert!`s in the `solver` module whose invariants are already enforced by downstream validated constructors (`Level::new` for `contract.level` in `par.rs`, `TrickCountRow::new` for per-seat trick counts in `tricks.rs`). No behavior change in release builds.

### Added

- `TrickCountRow::try_new(n, e, s, w)` returns `Result<Self, InvalidTrickCount>`, rejecting any per-seat value outside `0..=13`, mirroring `Level::try_new`.
- `TrickCount` newtype (see the Changed entry above) — construct with `TrickCount::new` / `TrickCount::try_new`, unwrap with `get()` or via `u8::from` / `usize::from`.
- `NonEmptyStrainFlags` — a guaranteed-non-empty wrapper around `StrainFlags`, analogous to `NonZero<T>`. Constructable via `NonEmptyStrainFlags::new(flags)` (returns `Option`); the inner value is recovered with `.get()` or `StrainFlags::from(…)`. `Solver::solve_deals` now takes `NonEmptyStrainFlags` instead of `StrainFlags`, encoding the non-empty requirement in the type.
- `Solver::analyse_play` wraps `AnalysePlayBin` to trace double-dummy trick counts before and after each card of a play sequence. Companion types `PlayTrace` (starting `Board` plus played cards) and `PlayAnalysis` (declarer-view tricks for the starting position and after each card). Integration tests cover empty traces, optimal-card invariance, and the all-one-suit deal.
- `Solver::analyse_plays` wraps `AnalyseAllPlaysBin` to analyse multiple play traces in parallel.
- `Board::try_new` now detects revokes among its played cards and returns a new `BoardError::Revoke { position: RevokePosition }` variant instead of deferring the check to DDS (which would panic).
- `CurrentTrick` — a standalone type for a trick-in-progress carrying `trump`, `leader`, and 0–3 pairwise-distinct played cards. Constructable via `CurrentTrick::new(trump, leader)` or `CurrentTrick::from_slice(trump, leader, played)`, with incremental `try_push`. Shape and disjointness invariants are enforced once here and no longer re-checked by `Board::try_new`.
- `SystemInfo` now exposes the full set of fields from `DDSInfo`: `platform()` (OS as `Platform`), `num_bits()`, `compiler()` (as `Compiler`), `threading()` (model as `Threading`), `num_cores()`, `thread_sizes()`, and `system_string()`. `Display` delegates to `system_string()`. New enums `Platform`, `Compiler`, and `Threading` carry all variants documented in the DDS header.

### Internal

- The `_` wildcard arm in the par-decoding seat match (`src/solver/par.rs`) is
  replaced by an explicit `3 => Seat::West` arm followed by
  `_ => unreachable!()`, so a malformed `contract.seats` value from DDS causes
  an immediate panic instead of silently falling back to `Seat::West`.
- `PlayTraceBin::from(&[Card])` is replaced by an infallible
  `From<&ArrayVec<Card, 52>>` that the call sites now use. `PlayTraceBin`
  remains `pub(super)`; no public API change.
- The `From<sys::*>` impls for `TrickCountTable`, `PlayAnalysis`, `FoundPlays`,
  and `Par` now route every `c_int` narrowing through a new private
  `solver::ffi` module. Invalid DDS output continues to panic, but with
  descriptive messages (naming the offending field and expected range)
  instead of the previous mix of assertion failures and generic out-of-bounds
  panics. In particular, the `futureTricks.suit` indexing into `Suit::DESC`
  and the `parResultsMaster.contracts[].denom` indexing into a local strain
  array — both previously unchecked — are now explicit validations.

## [0.17.0] - 2026-04-20

### Removed

- **Breaking:** `SystemError` is removed. DDS solver functions now panic on irrecoverable DDS errors instead of returning `Err(SystemError)`.

### Changed

- **Breaking:** `ParseCardError`, `ParseHoldingError`, `ParseHandError`, and `ParseDealError` are now `#[non_exhaustive]` so future variants can be added without a major bump. Downstream exhaustive matches must add a `_ =>` arm.
- `TricksRow::hex` and `TricksTable::hex` now return named `TricksRowHex` / `TricksTableHex<T>` wrapper types instead of opaque `impl UpperHex`, making the return type storable and referable in rustdoc. Both constructors are `const`. The `UpperHex` contract is unchanged.
- `ParseCardError::Suit` and `ParseCardError::Rank` now carry distinct hint messages listing valid suit characters and rank characters respectively.
- `Solver::solve_deal` doctest now runs (no longer `no_run`), guarding the canonical entry point against silent API drift.

### Added

- `Display` for `Vulnerability`, completing the `Display` ↔ `FromStr` round-trip. Formats as `none`, `ns`, `ew`, or `both`.
- Regression tests covering an invalid-deal error path and `solve_deals` chunking across the internal `MAXNOOFBOARDS` boundary.
- Integration tests for `solve_board` and `solve_boards`.

## [0.16.0] - 2026-04-19

### Changed

- **Breaking:** `Solver::solve_board` and `Solver::solve_boards` now accept `Objective` instead of separate `Board` and `Target` arguments. `Objective` bundles a board position with its solving query, eliminating redundant paired parameters.
- `Penalty::from_str` is now case-insensitive, accepting `X`/`XX` in addition to `x`/`xx`, matching `Contract::from_str`.

## [0.15.0] - 2026-04-16

### Added

- Optional `serde` feature with bridge-friendly string forms for bids, contracts, and suits.
- Parsers for bids, contracts, and graphical suit emojis, plus assorted string-form parsers.
- `Contract::new` constructor and restored `Bid::new`.
- `Display` impl for `Level`.
- PBN-solving example.
- Expanded README and doctests on the main entry points.

### Changed

- Hardened FFI boundaries and demoted internal sanity checks from `assert!` to debug-only.
- Hardened string parsing against malformed input.
- Polished `Holding` with a `const` set constructor and inlined bit operations.
- Trust DDS to return a valid par-contract level instead of re-validating.
- Loosened dependency version requirements to be friendlier to downstream crates.
- Polished crate metadata and modernized CI.
- Simplified internal representation of `Card`.

### Removed

- `Card::new` (construct via the checked APIs or parsers instead).

### Fixed

- Broken `Deal` doctest.
- Broken links in README.
- Misleading multithreading TODO in `solve_board`.

## [0.14.0] - 2026-04-14

### Added

- Type-safe `Rank` with `new`/`try_new` constructor pair.
- Iteration over `Holding` (including reverse iteration and extra `HoldingIter` traits) and over `Hand`.
- `FromIterator<Card>` for `Hand`.
- Fallible `try_new` constructor pairs across the public types, including a checked `Card::new`, `Level::new`, and safer `Deal`/`Hand` constructors.
- Explicit thread-safety model for solvers.
- Iterator-based `fill_deals`, with a documented warning that `fill_n_filtered_deals` may loop indefinitely.
- `Deck::drain` (Rustier rename).

### Changed

- Large internal refactor and resolved API inconsistencies.
- Replaced the `regex` dependency with a plain parser.
- Hoisted tightly coupled `Suit` and `Strain` to the crate root and curated re-exports.
- `Seat` arithmetic is now safe and `const`, with `Seat::letter` aligned to the other type conventions.
- `ParContract` made self-documenting; `None` doubling state renamed to `Undoubled` to avoid shadowing.
- Replaced `[Option<T>; N]` storage with `arrayvec`.
- Fallible conversions now use `TryFrom` instead of ad-hoc constructors.
- `Solver::try_lock` is now non-blocking.

### Removed

- Unsafe solvers from the public API.
- Arbitrary `Seat` arithmetic.
- `SystemError::Success` (paradoxical) and reuse of the reserved `SystemError` name.
- `Call` type and the heavy `deck` module (moved to `pons`).
- Internal `Strain::SYS`, thin `SmallSet` methods, obsolete compile-time checks, and unnecessary `#[inline]` hints.
- Non-structural `PartialEq` impls.

### Fixed

- Documentation fixes.

[0.18.0]: https://github.com/jdh8/dds-bridge/releases/tag/0.18.0
[0.17.0]: https://github.com/jdh8/dds-bridge/releases/tag/0.17.0
[0.16.0]: https://github.com/jdh8/dds-bridge/releases/tag/0.16.0
[0.15.0]: https://github.com/jdh8/dds-bridge/releases/tag/0.15.0
[0.14.0]: https://github.com/jdh8/dds-bridge/releases/tag/0.14.0
