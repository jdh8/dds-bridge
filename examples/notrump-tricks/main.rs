use dds_bridge::contract::Strain;
use dds_bridge::deal::{Deal, Seat};
use dds_bridge::solver;
use std::process::ExitCode;

/// Histogram of notrump tricks
#[derive(Debug, Clone, Copy, Default)]
struct Histogram {
    /// Histogram of notrump tricks for each player
    each: [u32; 14],
    /// Histogram of right-sided notrump tricks for each pair
    right: [u32; 14],
    /// Histogram of maximum notrump tricks for each deal
    max: [u32; 14],
}

fn to_cumulative_probability(histogram: [u32; 14]) -> [f64; 14] {
    let mut acc = 0;
    let mut cumsum = [0; 14];
    for (i, x) in histogram.into_iter().rev().enumerate() {
        acc += x;
        cumsum[13 - i] = acc;
    }
    cumsum.map(|x| f64::from(x) / f64::from(cumsum[0]))
}

fn analyze_deals(n: usize) -> Result<(), solver::Error> {
    let deals: Vec<_> = core::iter::repeat_with(|| Deal::new(&mut rand::thread_rng()))
        .take(n)
        .collect();

    let histogram = solver::solve_deals(&deals, solver::StrainFlags::NOTRUMP)?
        .into_iter()
        .map(|table| table[Strain::Notrump])
        .fold(Histogram::default(), |mut acc, row| {
            let (n, e, s, w) = (
                usize::from(row.get(Seat::North)),
                usize::from(row.get(Seat::East)),
                usize::from(row.get(Seat::South)),
                usize::from(row.get(Seat::West)),
            );
            acc.each[n] += 1;
            acc.each[e] += 1;
            acc.each[s] += 1;
            acc.each[w] += 1;
            acc.right[n.max(s)] += 1;
            acc.right[e.max(w)] += 1;
            acc.max[n.max(e).max(s).max(w)] += 1;
            acc
        });

    dbg!(&to_cumulative_probability(histogram.each)[6..]);
    dbg!(&to_cumulative_probability(histogram.right)[6..]);
    dbg!(&to_cumulative_probability(histogram.max)[6..]);
    Ok(())
}

#[doc = include_str!("README.md")]
fn main() -> Result<ExitCode, solver::Error> {
    match std::env::args().nth(1) {
        Some(string) => {
            if let Ok(n) = string.parse::<usize>() {
                analyze_deals(n)
            } else {
                eprintln!("{}", include_str!("README.md"));
                return Ok(ExitCode::FAILURE);
            }
        }
        None => analyze_deals(100),
    }?;
    Ok(ExitCode::SUCCESS)
}
