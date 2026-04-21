# dds-bridge

[![Build Status](https://github.com/jdh8/dds-bridge/actions/workflows/rust.yml/badge.svg)](https://github.com/jdh8/dds-bridge/actions/workflows/rust.yml)
[![Crates.io](https://img.shields.io/crates/v/dds-bridge.svg)](https://crates.io/crates/dds-bridge)
[![Docs.rs](https://docs.rs/dds-bridge/badge.svg)](https://docs.rs/dds-bridge)

A Rusty API for DDS, the double-dummy solver for the game of bridge.
This crate aims to be the building block of computer bridge in Rust.
It links to [`dds-bridge/dds`][dds], the fundamental C++ double dummy
solver, via [our system crate][sys].

[dds]: https://github.com/dds-bridge/dds
[sys]: https://docs.rs/dds-bridge-sys

## Installation

```sh
cargo add dds-bridge
```

A C++ toolchain is required to build the underlying `dds-bridge-sys`
crate.  On most Linux distributions the default `g++` and `make` are
enough; macOS needs the Xcode Command Line Tools; Windows builds via
MSVC.

## Quick start

Parse a [PBN-formatted][pbn] deal, solve it for all strains and seats,
and score a specific contract:

```rust
use dds_bridge::{Contract, FullDeal, Penalty, Solver, Strain, Seat};

# fn main() -> Result<(), Box<dyn std::error::Error>> {
// Each player holds a 13-card straight flush in one suit.
let deal: FullDeal = "N:AKQJT98765432... .AKQJT98765432.. \
                      ..AKQJT98765432. ...AKQJT98765432".parse()?;

let solver = Solver::lock();
let tricks = solver.solve_deal(deal);

// 4♠ by North, scored as if 10 tricks taken, not vulnerable.
let contract = Contract::new(4, Strain::Spades, Penalty::Undoubled);
assert_eq!(contract.score(10, false), 420);
# Ok(())
# }
```

[pbn]: https://www.tistis.nl/pbn/

## Features

- `serde` *(optional)* — `Serialize`/`Deserialize` for all public types.
  Human-readable formats (JSON, YAML, TOML) round-trip through the
  [`Display`][Display]/[`FromStr`][FromStr] text form; binary formats use a
  compact derived representation.

[Display]: https://doc.rust-lang.org/std/fmt/trait.Display.html
[FromStr]: https://doc.rust-lang.org/std/str/trait.FromStr.html

## Minimum supported Rust version

Rust **1.85** (the first release supporting Rust 2024 edition).

## License

Licensed under the [Apache License, Version 2.0][license].

[license]: https://github.com/jdh8/dds-bridge/blob/main/LICENSE
