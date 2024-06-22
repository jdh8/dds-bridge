use crate::*;
use dds_bridge_sys as sys;

const _: () = assert!(core::mem::size_of::<Option<Play>>() == core::mem::size_of::<Play>());

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
    const CONTRACTS: [(Contract, Seat, i8); 2] = [
        (
            Contract::new(7, Strain::Spades, Penalty::None),
            Seat::East,
            0,
        ),
        (
            Contract::new(7, Strain::Spades, Penalty::None),
            Seat::West,
            0,
        ),
    ];
    let ns = Par {
        score: -2210,
        contracts: CONTRACTS.to_vec(),
    };
    let ew = Par {
        score: 2210,
        contracts: CONTRACTS.to_vec(),
    };
    assert_eq!(
        solve_deal(DEAL).expect("Failed to solve the deal"),
        SOLUTION
    );
    assert_eq!(
        calculate_pars(SOLUTION, Vulnerability::all()).expect("Failed to calculate par scores"),
        [ns, ew]
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
    const PAR: Par = Par {
        score: 0,
        contracts: Vec::new(),
    };
    assert_eq!(
        solve_deal(DEAL).expect("Failed to solve the deal"),
        SOLUTION
    );
    assert_eq!(
        calculate_pars(SOLUTION, Vulnerability::all()).expect("Failed to calculate par scores"),
        [PAR; 2]
    );
}

/// A symmetric deal where everyone makes 1NT but no suit contract
///
/// This example is taken from
/// <http://www.rpbridge.net/7a23.htm#2>.
#[test]
#[allow(clippy::unusual_byte_groupings)]
fn solve_everyone_makes_1nt() {
    const A54: Holding = Holding::from_bits(0b10000_0000_1100_00);
    const QJ32: Holding = Holding::from_bits(0b00110_0000_0011_00);
    const K976: Holding = Holding::from_bits(0b01000_1011_0000_00);
    const T8: Holding = Holding::from_bits(0b00001_0100_0000_00);
    const DEAL: Deal = Deal([
        Hand([A54, QJ32, K976, T8]),
        Hand([T8, A54, QJ32, K976]),
        Hand([K976, T8, A54, QJ32]),
        Hand([QJ32, K976, T8, A54]),
    ]);
    const SUIT: TricksRow = TricksRow::new(6, 6, 6, 6);
    const NT: TricksRow = TricksRow::new(7, 7, 7, 7);
    const SOLUTION: TricksTable = TricksTable([SUIT, SUIT, SUIT, SUIT, NT]);
    const CONTRACT: Contract = Contract::new(1, Strain::Notrump, Penalty::None);
    assert_eq!(
        solve_deal(DEAL).expect("Failed to solve the deal"),
        SOLUTION
    );

    let ns = Par {
        score: 90,
        contracts: vec![(CONTRACT, Seat::North, 0), (CONTRACT, Seat::South, 0)],
    };
    let ew = Par {
        score: 90,
        contracts: vec![(CONTRACT, Seat::East, 0), (CONTRACT, Seat::West, 0)],
    };
    assert_eq!(
        calculate_pars(SOLUTION, Vulnerability::all()).expect("Failed to calculate par scores"),
        [ns, ew]
    );
}
