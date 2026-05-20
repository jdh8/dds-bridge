//! Benchmarks for the batch solver entry points.
//!
//! Run with the default thread pool, or set `RAYON_NUM_THREADS=1` to measure
//! the single-threaded baseline that DDS 3.0's stripped-down batch API forces
//! on callers without a per-worker `SolverContext`.

use arrayvec::ArrayVec;
use criterion::{Criterion, criterion_group, criterion_main};
use dds_bridge::solver::{
    Board, CurrentTrick, NonEmptyStrainFlags, Objective, PlayTrace, Solver, Target,
};
use dds_bridge::{Builder, FullDeal, Hand, Holding, PartialDeal, Seat, Strain};
use rand::SeedableRng;
use rand::rngs::SmallRng;
use rand::seq::SliceRandom;
use std::hint::black_box;

const N: usize = 32;

/// `N` deterministic random deals from a seeded RNG.
fn deals(seed: u64) -> Vec<FullDeal> {
    let mut rng = SmallRng::seed_from_u64(seed);
    (0..N)
        .map(|_| {
            #[allow(clippy::cast_possible_truncation)]
            let mut cards: [u8; 52] = core::array::from_fn(|i| i as u8);
            cards.shuffle(&mut rng);
            let to_hand = |slice: &[u8]| {
                let mut bits = [0u16; 4];
                for &c in slice {
                    bits[(c / 13) as usize] |= 1 << (c % 13 + 2);
                }
                Hand::new(
                    Holding::from_bits_truncate(bits[0]),
                    Holding::from_bits_truncate(bits[1]),
                    Holding::from_bits_truncate(bits[2]),
                    Holding::from_bits_truncate(bits[3]),
                )
            };
            Builder::new()
                .north(to_hand(&cards[..13]))
                .east(to_hand(&cards[13..26]))
                .south(to_hand(&cards[26..39]))
                .west(to_hand(&cards[39..]))
                .build_full()
                .expect("deal is valid by construction")
        })
        .collect()
}

fn board_from(deal: FullDeal) -> Board {
    let remaining = PartialDeal::from(deal);
    Board::try_new(remaining, CurrentTrick::new(Strain::Notrump, Seat::North))
        .expect("start-of-trick NT board")
}

fn bench_solve_deals(c: &mut Criterion) {
    let solver = Solver::lock();
    let ds = deals(0);
    c.bench_function("solve_deals_32", |b| {
        b.iter(|| black_box(solver.solve_deals(black_box(&ds), NonEmptyStrainFlags::ALL)));
    });
}

fn bench_solve_boards(c: &mut Criterion) {
    let solver = Solver::lock();
    let objectives: Vec<Objective> = deals(1)
        .into_iter()
        .map(|d| Objective {
            board: board_from(d),
            target: Target::Any(None),
        })
        .collect();
    c.bench_function("solve_boards_32", |b| {
        b.iter(|| black_box(solver.solve_boards(black_box(&objectives))));
    });
}

fn bench_analyse_plays(c: &mut Criterion) {
    let solver = Solver::lock();
    let traces: Vec<PlayTrace> = deals(2)
        .into_iter()
        .map(|d| PlayTrace {
            board: board_from(d),
            cards: ArrayVec::new(),
        })
        .collect();
    c.bench_function("analyse_plays_32", |b| {
        b.iter(|| black_box(solver.analyse_plays(black_box(&traces))));
    });
}

criterion_group!(
    benches,
    bench_solve_deals,
    bench_solve_boards,
    bench_analyse_plays,
);
criterion_main!(benches);
