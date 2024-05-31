use super::*;
use crate::deal::Deal;
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
