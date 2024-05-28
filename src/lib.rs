//! This crate aims to be the building block of computer bridge in Rust.
//! This crate links to [`dds-bridge/dds`][dds], the fundamental C++ double
//! dummy solver, via [our system crate][sys].
//!
//! [dds]: https://github.com/dds-bridge/dds
//! [sys]: https://lib.rs/crates/dds-bridge-sys

#![warn(missing_docs)]

/// Data structures about bidding and scoring
pub mod contract;

/// Data structures about dealing and playing
pub mod deal;

/// Solver functions for double dummy problems
pub mod solver;
