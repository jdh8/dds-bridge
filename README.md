dds-bridge
==========
![Build Status](https://github.com/jdh8/dds-bridge/actions/workflows/rust.yml/badge.svg)
[![Crates.io](https://img.shields.io/crates/v/dds-bridge.svg)](https://crates.io/crates/dds-bridge)
[![Docs.rs](https://docs.rs/dds-bridge/badge.svg)](https://docs.rs/dds-bridge)

This crate aims to be the building block of computer bridge in Rust.
This crate links to [`dds-bridge/dds`][dds], the fundamental C++ double
dummy solver, via [our system crate][sys].

[dds]: https://github.com/dds-bridge/dds
[sys]: https://lib.rs/crates/dds-bridge-sys
