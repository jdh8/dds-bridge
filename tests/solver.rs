use dds_bridge::solver::*;
use dds_bridge::{Bid, Contract, Deal, Hand, Holding, Penalty, Seat, Strain};

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
    const CONTRACT: Contract = Contract {
        bid: Bid {
            level: dds_bridge::Level::new(7),
            strain: Strain::Spades,
        },
        penalty: Penalty::Undoubled,
    };
    const CONTRACTS: [ParContract; 2] = [
        ParContract {
            contract: CONTRACT,
            declarer: Seat::East,
            overtricks: 0,
        },
        ParContract {
            contract: CONTRACT,
            declarer: Seat::West,
            overtricks: 0,
        },
    ];
    let ns = Par {
        score: -2210,
        contracts: CONTRACTS.to_vec(),
    };
    let ew = Par {
        score: 2210,
        contracts: CONTRACTS.to_vec(),
    };
    assert_eq!(Solver::lock().solve_deal(DEAL).unwrap(), SOLUTION);
    let pars = calculate_pars(SOLUTION, Vulnerability::all()).unwrap();
    assert!(pars[0].equivalent(&ns));
    assert!(pars[1].equivalent(&ew));
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
    assert_eq!(Solver::lock().solve_deal(DEAL).unwrap(), SOLUTION);
    let pars = calculate_pars(SOLUTION, Vulnerability::all()).unwrap();
    assert!(pars[0].equivalent(&PAR));
    assert!(pars[1].equivalent(&PAR));
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
        bid: Bid {
            level: dds_bridge::Level::new(1),
            strain: Strain::Notrump,
        },
        penalty: Penalty::Undoubled,
    };
    assert_eq!(Solver::lock().solve_deal(DEAL).unwrap(), SOLUTION);
    let ns = Par {
        score: 90,
        contracts: vec![
            ParContract {
                contract: CONTRACT,
                declarer: Seat::North,
                overtricks: 0,
            },
            ParContract {
                contract: CONTRACT,
                declarer: Seat::South,
                overtricks: 0,
            },
        ],
    };
    let ew = Par {
        score: 90,
        contracts: vec![
            ParContract {
                contract: CONTRACT,
                declarer: Seat::East,
                overtricks: 0,
            },
            ParContract {
                contract: CONTRACT,
                declarer: Seat::West,
                overtricks: 0,
            },
        ],
    };
    let pars = calculate_pars(SOLUTION, Vulnerability::all()).unwrap();
    assert!(pars[0].equivalent(&ns));
    assert!(pars[1].equivalent(&ew));
}
