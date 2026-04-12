use dds_bridge::Strain;
use dds_bridge::contract::{Bid, Contract, Penalty};
use dds_bridge::deal::{Deal, Hand, Holding, Seat, SmallSet as _};
use dds_bridge::deck::full_deal;
use dds_bridge::solver::*;
use dds_bridge_sys as sys;

const _: () = assert!(core::mem::size_of::<Option<Play>>() == core::mem::size_of::<Play>());

#[test]
fn test_solving_deals() {
    const N: usize = sys::MAXNOOFBOARDS as usize * 2;
    let deals: [_; N] = core::array::from_fn(|_| full_deal(&mut rand::rng()));
    let solver = Solver::new();
    let array = deals.map(|x| solver.solve_deal(x).expect("Failed to solve a deal"));
    let vec = solver
        .solve_deals(&deals, StrainFlags::all())
        .expect("Failed to solve all deals");
    assert_eq!(array, vec.as_slice());
}

/// Everyone has a 13-card straight flush, and the par is 7SW=.
#[test]
fn solve_four_13_card_straight_flushes() {
    const DEAL: Deal = Deal::new(
        Hand::new(Holding::ALL, Holding::EMPTY, Holding::EMPTY, Holding::EMPTY),
        Hand::new(Holding::EMPTY, Holding::ALL, Holding::EMPTY, Holding::EMPTY),
        Hand::new(Holding::EMPTY, Holding::EMPTY, Holding::ALL, Holding::EMPTY),
        Hand::new(Holding::EMPTY, Holding::EMPTY, Holding::EMPTY, Holding::ALL),
    );
    const SOLUTION: TricksTable = TricksTable([
        TricksRow::new(13, 0, 13, 0),
        TricksRow::new(0, 13, 0, 13),
        TricksRow::new(13, 0, 13, 0),
        TricksRow::new(0, 13, 0, 13),
        TricksRow::new(0, 0, 0, 0),
    ]);
    const CONTRACTS: [(Contract, Seat, i8); 2] = [
        (
            Contract {
                bid: match Bid::new(7, Strain::Spades) {
                    Ok(b) => b,
                    Err(_) => panic!(),
                },
                penalty: Penalty::Undoubled,
            },
            Seat::East,
            0,
        ),
        (
            Contract {
                bid: match Bid::new(7, Strain::Spades) {
                    Ok(b) => b,
                    Err(_) => panic!(),
                },
                penalty: Penalty::Undoubled,
            },
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
        Solver::new()
            .solve_deal(DEAL)
            .expect("Failed to solve the deal"),
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
    const AKQJ: Holding = Holding::from_bits_truncate(0xF << 11);
    const T987: Holding = Holding::from_bits_truncate(0xF << 7);
    const XXXX: Holding = Holding::from_bits_truncate(0xF << 3);
    const X: Holding = Holding::from_bits_truncate(1 << 2);
    const DEAL: Deal = Deal::new(
        Hand::new(T987, XXXX, X, AKQJ),
        Hand::new(X, AKQJ, T987, XXXX),
        Hand::new(XXXX, T987, AKQJ, X),
        Hand::new(AKQJ, X, XXXX, T987),
    );
    const SOLUTION: TricksTable = TricksTable([TricksRow::new(5, 5, 5, 5); 5]);
    const PAR: Par = Par {
        score: 0,
        contracts: Vec::new(),
    };
    assert_eq!(
        Solver::new()
            .solve_deal(DEAL)
            .expect("Failed to solve the deal"),
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
    const A54: Holding = Holding::from_bits_truncate(0b10000_0000_1100_00);
    const QJ32: Holding = Holding::from_bits_truncate(0b00110_0000_0011_00);
    const K976: Holding = Holding::from_bits_truncate(0b01000_1011_0000_00);
    const T8: Holding = Holding::from_bits_truncate(0b00001_0100_0000_00);
    const DEAL: Deal = Deal::new(
        Hand::new(A54, QJ32, K976, T8),
        Hand::new(T8, A54, QJ32, K976),
        Hand::new(K976, T8, A54, QJ32),
        Hand::new(QJ32, K976, T8, A54),
    );
    const SUIT: TricksRow = TricksRow::new(6, 6, 6, 6);
    const NT: TricksRow = TricksRow::new(7, 7, 7, 7);
    const SOLUTION: TricksTable = TricksTable([SUIT, SUIT, SUIT, SUIT, NT]);
    const CONTRACT: Contract = Contract {
        bid: match Bid::new(1, Strain::Notrump) {
            Ok(b) => b,
            Err(_) => panic!(),
        },
        penalty: Penalty::Undoubled,
    };
    assert_eq!(
        Solver::new()
            .solve_deal(DEAL)
            .expect("Failed to solve the deal"),
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
