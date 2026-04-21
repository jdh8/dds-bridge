//! Solve double-dummy tricks for every deal in one or more PBN inputs.
//!
//! Usage:
//!   cargo run --example solve-pbn -- file1.pbn file2.pbn ...
//!   cat file.pbn | cargo run --example solve-pbn
//!
//! If a string doesn't contain any `[Deal "..."]` tag, the whole trimmed input
//! is tried as a single deal (e.g. `N:AKQJT98765432... ...`).

use dds_bridge::solver::NonEmptyStrainFlags;
use dds_bridge::{FullDeal, Seat, Solver, Strain};
use std::io::{self, Read};
use std::process::ExitCode;

fn extract_deals(input: &str) -> Vec<&str> {
    const TAG: &str = "[Deal \"";
    let mut deals = Vec::new();
    let mut rest = input;
    while let Some(start) = rest.find(TAG) {
        rest = &rest[start + TAG.len()..];
        if let Some(end) = rest.find('"') {
            deals.push(rest[..end].trim());
            rest = &rest[end + 1..];
        } else {
            break;
        }
    }
    if deals.is_empty() {
        let trimmed = input.trim();
        if !trimmed.is_empty() {
            deals.push(trimmed);
        }
    }
    deals
}

fn read_sources(args: &[String]) -> io::Result<String> {
    if args.is_empty() {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf)?;
        return Ok(buf);
    }
    let mut buf = String::new();
    for path in args {
        buf.push_str(&std::fs::read_to_string(path)?);
        buf.push('\n');
    }
    Ok(buf)
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let input = match read_sources(&args) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::FAILURE;
        }
    };

    let raw = extract_deals(&input);
    if raw.is_empty() {
        eprintln!("error: no deals found");
        return ExitCode::FAILURE;
    }

    let mut deals = Vec::with_capacity(raw.len());
    for (i, s) in raw.iter().enumerate() {
        match s.parse::<FullDeal>() {
            Ok(d) => deals.push(d),
            Err(e) => {
                eprintln!("deal {}: parse error: {e}", i + 1);
                return ExitCode::FAILURE;
            }
        }
    }

    let tables = Solver::lock().solve_deals(&deals, NonEmptyStrainFlags::ALL);

    for (i, (deal, table)) in raw.iter().zip(&tables).enumerate() {
        println!("Deal {}: {deal}", i + 1);
        println!("     {:>4} {:>4} {:>4} {:>4}", "N", "E", "S", "W");
        for strain in Strain::ASC {
            let row = table[strain];
            let label = match strain {
                Strain::Notrump => "NT".to_string(),
                _ => strain.to_string(),
            };
            println!(
                "  {label:>2} {:>4} {:>4} {:>4} {:>4}",
                row.get(Seat::North),
                row.get(Seat::East),
                row.get(Seat::South),
                row.get(Seat::West),
            );
        }
        println!();
    }

    ExitCode::SUCCESS
}
