# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- markdownlint-disable no-duplicate-heading -->

## [Unreleased]

### Added

- `Solver::analyse_play` wraps `AnalysePlayBin` to trace double-dummy trick counts before and after each card of a play sequence. Companion types `PlayTrace` (starting `Board` plus played cards) and `PlayAnalysis` (declarer-view tricks for the starting position and after each card). Integration tests cover empty traces, optimal-card invariance, and the all-one-suit deal.
- `Solver::analyse_plays` wraps `AnalyseAllPlaysBin` to analyse multiple play traces in parallel.
- `SystemInfo` now exposes the full set of fields from `DDSInfo`: `platform()` (OS as `Platform`), `num_bits()`, `compiler()` (as `Compiler`), `threading()` (model as `Threading`), `num_cores()`, `thread_sizes()`, and `system_string()`. `Display` delegates to `system_string()`. New enums `Platform`, `Compiler`, and `Threading` carry all variants documented in the DDS header.

## [0.17.0]

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

[Unreleased]: https://github.com/jdh8/dds-bridge/compare/v0.17.0...HEAD
[0.17.0]: https://github.com/jdh8/dds-bridge/releases/tag/0.17.0
[0.16.0]: https://github.com/jdh8/dds-bridge/releases/tag/0.16.0
[0.15.0]: https://github.com/jdh8/dds-bridge/releases/tag/0.15.0
[0.14.0]: https://github.com/jdh8/dds-bridge/releases/tag/0.14.0
