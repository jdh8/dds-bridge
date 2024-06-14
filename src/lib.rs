//! This crate aims to be the building block of computer bridge in Rust.
//! This crate links to [`dds-bridge/dds`][dds], the fundamental C++ double
//! dummy solver, via [our system crate][sys].
//!
//! [dds]: https://github.com/dds-bridge/dds
//! [sys]: https://lib.rs/crates/dds-bridge-sys
#![warn(missing_docs)]

mod contract;
mod deal;
mod solver;

pub use contract::*;
pub use deal::*;
pub use solver::*;
