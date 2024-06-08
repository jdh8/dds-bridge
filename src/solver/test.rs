use super::*;
use crate::deal::{Deal, Hand, SmallSet};
use dds_bridge_sys as sys;

#[test]
fn test_solving_deals() {
    const N: usize = sys::MAXNOOFBOARDS as usize * 2;
    let deals: [_; N] = core::array::from_fn(|_| Deal::new(&mut rand::thread_rng()));
    let array = deals.map(|x| solve_deal(x).expect("Failed to solve a deal"));
    let vec = solve_deals(&deals, StrainFlags::all()).expect("Failed to solve all deals");
    assert_eq!(array, vec.as_slice());
}

#[test]
fn test_solving_segment() {
    const FLAGS: StrainFlags = StrainFlags::SPADES.union(StrainFlags::HEARTS);
    const N: usize = (sys::MAXNOOFBOARDS / FLAGS.bits().count_ones()) as usize;
    let deals: [_; N] = core::array::from_fn(|_| Deal::new(&mut rand::thread_rng()));
    let vec = solve_deals(&deals, FLAGS).expect("Failed to solve all deals");
    let result = unsafe { solve_deal_segment(&deals, FLAGS).expect("Failed to solve all deals") };
    let result: [_; N] = result.results[..N].try_into().expect("Not enough results");
    let result = result.map(TricksTable::from);
    assert_eq!(vec.as_slice(), result);
}

/// Everyone has a 13-card straight flush, and the par is 7SW=.
#[test]
fn solve_four_13_card_straight_flushes() {
    const DEAL: Deal = Deal([
        Hand([Holding::ALL, Holding::EMPTY, Holding::EMPTY, Holding::EMPTY]),
        Hand([Holding::EMPTY, Holding::ALL, Holding::EMPTY, Holding::EMPTY]),
        Hand([Holding::EMPTY, Holding::EMPTY, Holding::ALL, Holding::EMPTY]),
        Hand([Holding::EMPTY, Holding::EMPTY, Holding::EMPTY, Holding::ALL]),
    ]);
    const SOLUTION: TricksTable = TricksTable([
        TricksRow::new(13, 0, 13, 0),
        TricksRow::new(0, 13, 0, 13),
        TricksRow::new(13, 0, 13, 0),
        TricksRow::new(0, 13, 0, 13),
        TricksRow::new(0, 0, 0, 0),
    ]);
    assert_eq!(
        solve_deal(DEAL).expect("Failed to solve the deal"),
        SOLUTION
    );
}

/// Defenders can cash 8 tricks in every strain.
///
/// This example is taken from
/// <http://bridge.thomasoandrews.com/deals/parzero/>.
#[test]
fn solve_par_5_tricks() {
    const AKQJ: Holding = Holding::from_bits(0xF << 11);
    const T987: Holding = Holding::from_bits(0xF << 7);
    const XXXX: Holding = Holding::from_bits(0xF << 3);
    const X: Holding = Holding::from_bits(1 << 2);
    const DEAL: Deal = Deal([
        Hand([T987, XXXX, X, AKQJ]),
        Hand([X, AKQJ, T987, XXXX]),
        Hand([XXXX, T987, AKQJ, X]),
        Hand([AKQJ, X, XXXX, T987]),
    ]);
    const SOLUTION: TricksTable = TricksTable([TricksRow::new(5, 5, 5, 5); 5]);
    assert_eq!(
        solve_deal(DEAL).expect("Failed to solve the deal"),
        SOLUTION
    );
}
