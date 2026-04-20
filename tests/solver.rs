use arrayvec::ArrayVec;
use dds_bridge::solver::*;
use dds_bridge::{Card, Contract, Deal, Hand, Holding, Penalty, Rank, Seat, Strain, Suit};

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
    const CONTRACT: Contract = Contract::new(7, Strain::Spades, Penalty::Undoubled);
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
    assert_eq!(Solver::lock().solve_deal(DEAL), SOLUTION);

    let pars = calculate_pars(SOLUTION, Vulnerability::all());
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
    assert_eq!(Solver::lock().solve_deal(DEAL), SOLUTION);

    let pars = calculate_pars(SOLUTION, Vulnerability::all());
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
    const CONTRACT: Contract = Contract::new(1, Strain::Notrump, Penalty::Undoubled);
    assert_eq!(Solver::lock().solve_deal(DEAL), SOLUTION);

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
    let pars = calculate_pars(SOLUTION, Vulnerability::all());
    assert!(pars[0].equivalent(&ns));
    assert!(pars[1].equivalent(&ew));
}

/// `solve_board` scores agree with the double-dummy table for the same deal.
///
/// This uses the symmetric 1NT deal from `solve_everyone_makes_1nt`, where
/// every seat makes exactly 7 NT tricks as declarer.  With multiple legal moves
/// available, DDS computes actual scores.
///
/// When North leads, West is North's natural declarer (the player to North's
/// right).  West makes 7 tricks, so NS as the leading/defending side makes 6.
#[test]
fn solve_board_score_matches_dd_table() {
    // Same deal as `solve_everyone_makes_1nt`
    const A54: Holding = Holding::from_bits_truncate(0b100_0000_0011_0000);
    const QJ32: Holding = Holding::from_bits_truncate(0b001_1000_0000_1100);
    const K976: Holding = Holding::from_bits_truncate(0b010_0010_1100_0000);
    const T8: Holding = Holding::from_bits_truncate(0b000_0101_0000_0000);
    const DEAL: Deal = Deal::new(
        Hand::new(A54, QJ32, K976, T8),
        Hand::new(T8, A54, QJ32, K976),
        Hand::new(K976, T8, A54, QJ32),
        Hand::new(QJ32, K976, T8, A54),
    );
    let solver = Solver::lock();
    let tricks = solver.solve_deal(DEAL);
    let found = solver.solve_board(Objective {
        board: Board {
            trump: Strain::Notrump,
            lead: Seat::North,
            current_cards: ArrayVec::new(),
            remaining: DEAL,
        },
        target: Target::Any(-1),
    });
    core::mem::drop(solver);
    // solve_board reports tricks for the leading side (NS as defenders here).
    // The declarer is North's RHO (West).  Defenders take 13 - declarer's tricks.
    let expected = 13 - tricks[Strain::Notrump].get(Seat::North.rho());
    assert!(!found.plays.is_empty());
    assert_eq!(i32::from(found.plays[0].score), i32::from(expected));
}

/// `solve_boards` returns the same results as individual `solve_board` calls.
#[test]
fn solve_boards_matches_solve_board() {
    const A54: Holding = Holding::from_bits_truncate(0b100_0000_0011_0000);
    const QJ32: Holding = Holding::from_bits_truncate(0b001_1000_0000_1100);
    const K976: Holding = Holding::from_bits_truncate(0b010_0010_1100_0000);
    const T8: Holding = Holding::from_bits_truncate(0b000_0101_0000_0000);
    const DEAL: Deal = Deal::new(
        Hand::new(A54, QJ32, K976, T8),
        Hand::new(T8, A54, QJ32, K976),
        Hand::new(K976, T8, A54, QJ32),
        Hand::new(QJ32, K976, T8, A54),
    );
    let solver = Solver::lock();
    let obj = Objective {
        board: Board {
            trump: Strain::Notrump,
            lead: Seat::North,
            current_cards: ArrayVec::new(),
            remaining: DEAL,
        },
        target: Target::Any(-1),
    };
    let single = solver.solve_board(obj.clone());
    let batch = solver.solve_boards(&[obj]);
    core::mem::drop(solver);
    assert_eq!(batch.len(), 1);
    // Node counts differ between single and batch solvers; compare only the plays.
    assert_eq!(batch[0].plays, single.plays);
}

/// `solve_deals` must chunk transparently across the internal `MAXNOOFBOARDS`
/// boundary.  With 5 strains and `MAXNOOFBOARDS == 200`, each chunk holds 40
/// deals, so 41 identical deals force a second chunk; every result must equal
/// the single-deal answer.
#[test]
fn solve_deals_crosses_chunk_boundary() {
    const DEAL: Deal = Deal::new(
        Hand::new(Holding::ALL, Holding::EMPTY, Holding::EMPTY, Holding::EMPTY),
        Hand::new(Holding::EMPTY, Holding::ALL, Holding::EMPTY, Holding::EMPTY),
        Hand::new(Holding::EMPTY, Holding::EMPTY, Holding::ALL, Holding::EMPTY),
        Hand::new(Holding::EMPTY, Holding::EMPTY, Holding::EMPTY, Holding::ALL),
    );
    let solver = Solver::lock();
    let expected = solver.solve_deal(DEAL);

    let deals = [DEAL; 41];
    let tables = solver.solve_deals(&deals, StrainFlags::all());
    core::mem::drop(solver);

    assert_eq!(tables.len(), deals.len());
    assert!(tables.iter().all(|&t| t == expected));
}

/// `analyse_play` with an empty trace returns just the starting DD value.
///
/// DDS reports tricks from declarer's viewpoint — declarer is the RHO of the
/// opening leader — whereas `solve_board` reports tricks for the leading
/// side, so the two values must sum to 13.
#[test]
fn analyse_play_empty_trace_complements_solve_board() {
    const A54: Holding = Holding::from_bits_truncate(0b100_0000_0011_0000);
    const QJ32: Holding = Holding::from_bits_truncate(0b001_1000_0000_1100);
    const K976: Holding = Holding::from_bits_truncate(0b010_0010_1100_0000);
    const T8: Holding = Holding::from_bits_truncate(0b000_0101_0000_0000);
    const DEAL: Deal = Deal::new(
        Hand::new(A54, QJ32, K976, T8),
        Hand::new(T8, A54, QJ32, K976),
        Hand::new(K976, T8, A54, QJ32),
        Hand::new(QJ32, K976, T8, A54),
    );
    let board = Board {
        trump: Strain::Notrump,
        lead: Seat::North,
        current_cards: ArrayVec::new(),
        remaining: DEAL,
    };
    let solver = Solver::lock();
    let found = solver.solve_board(Objective {
        board: board.clone(),
        target: Target::Any(-1),
    });
    let analysis = solver.analyse_play(PlayTrace {
        board,
        cards: ArrayVec::new(),
    });
    core::mem::drop(solver);
    assert_eq!(analysis.tricks.len(), 1);
    assert_eq!(
        i32::from(analysis.tricks[0]) + i32::from(found.plays[0].score),
        13,
    );
}

/// Playing a card that `solve_board` ranks first must preserve the
/// declarer-side DD value across the card.
#[test]
fn analyse_play_optimal_card_preserves_dd_value() {
    const A54: Holding = Holding::from_bits_truncate(0b100_0000_0011_0000);
    const QJ32: Holding = Holding::from_bits_truncate(0b001_1000_0000_1100);
    const K976: Holding = Holding::from_bits_truncate(0b010_0010_1100_0000);
    const T8: Holding = Holding::from_bits_truncate(0b000_0101_0000_0000);
    const DEAL: Deal = Deal::new(
        Hand::new(A54, QJ32, K976, T8),
        Hand::new(T8, A54, QJ32, K976),
        Hand::new(K976, T8, A54, QJ32),
        Hand::new(QJ32, K976, T8, A54),
    );
    let board = Board {
        trump: Strain::Notrump,
        lead: Seat::North,
        current_cards: ArrayVec::new(),
        remaining: DEAL,
    };
    let solver = Solver::lock();
    let found = solver.solve_board(Objective {
        board: board.clone(),
        target: Target::Any(-1),
    });
    let best = found.plays[0];
    let mut cards = ArrayVec::new();
    cards.push(best.card);
    let analysis = solver.analyse_play(PlayTrace { board, cards });
    core::mem::drop(solver);
    assert_eq!(analysis.tricks.len(), 2);
    assert_eq!(analysis.tricks[0], analysis.tricks[1]);
    assert_eq!(i32::from(analysis.tricks[0]) + i32::from(best.score), 13,);
}

/// Straight-flush deal, NT contract: `Hand::new` orders suits C, D, H, S, so
/// North holds all clubs, East all diamonds, South all hearts, West all
/// spades.  North on lead runs every club trick, leaving declarer (North's
/// RHO, West) with zero — which must hold across the opening lead.
#[test]
fn analyse_play_straight_flush_declarer_takes_zero() {
    const DEAL: Deal = Deal::new(
        Hand::new(Holding::ALL, Holding::EMPTY, Holding::EMPTY, Holding::EMPTY),
        Hand::new(Holding::EMPTY, Holding::ALL, Holding::EMPTY, Holding::EMPTY),
        Hand::new(Holding::EMPTY, Holding::EMPTY, Holding::ALL, Holding::EMPTY),
        Hand::new(Holding::EMPTY, Holding::EMPTY, Holding::EMPTY, Holding::ALL),
    );
    let mut cards = ArrayVec::<Card, 52>::new();
    cards.push(Card {
        suit: Suit::Clubs,
        rank: Rank::A,
    });
    let analysis = Solver::lock().analyse_play(PlayTrace {
        board: Board {
            trump: Strain::Notrump,
            lead: Seat::North,
            current_cards: ArrayVec::new(),
            remaining: DEAL,
        },
        cards,
    });
    assert_eq!(analysis.tricks.len(), 2);
    assert!(analysis.tricks.iter().all(|&t| t == 0));
}
