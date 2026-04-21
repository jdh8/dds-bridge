use arrayvec::ArrayVec;
use dds_bridge::solver::*;
use dds_bridge::{Builder, Card, Contract, Hand, Holding, Penalty, Rank, Seat, Strain, Suit};
use semver::Version;

/// Everyone has a 13-card straight flush, and the par is 7SW=.
#[test]
fn solve_four_13_card_straight_flushes() {
    const DEAL: Builder = Builder::new()
        .north(Hand::new(
            Holding::ALL,
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::EMPTY,
        ))
        .east(Hand::new(
            Holding::EMPTY,
            Holding::ALL,
            Holding::EMPTY,
            Holding::EMPTY,
        ))
        .south(Hand::new(
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::ALL,
            Holding::EMPTY,
        ))
        .west(Hand::new(
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::ALL,
        ));
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
    assert_eq!(
        Solver::lock().solve_deal(DEAL.build_full().unwrap()),
        SOLUTION
    );

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
    const DEAL: Builder = Builder::new()
        .north(Hand::new(T987, XXXX, X, AKQJ))
        .east(Hand::new(X, AKQJ, T987, XXXX))
        .south(Hand::new(XXXX, T987, AKQJ, X))
        .west(Hand::new(AKQJ, X, XXXX, T987));
    const SOLUTION: TricksTable = TricksTable([TricksRow::new(5, 5, 5, 5); 5]);
    const PAR: Par = Par {
        score: 0,
        contracts: Vec::new(),
    };
    assert_eq!(
        Solver::lock().solve_deal(DEAL.build_full().unwrap()),
        SOLUTION
    );

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
    const DEAL: Builder = Builder::new()
        .north(Hand::new(A54, QJ32, K976, T8))
        .east(Hand::new(T8, A54, QJ32, K976))
        .south(Hand::new(K976, T8, A54, QJ32))
        .west(Hand::new(QJ32, K976, T8, A54));
    const SUIT: TricksRow = TricksRow::new(6, 6, 6, 6);
    const NT: TricksRow = TricksRow::new(7, 7, 7, 7);
    const SOLUTION: TricksTable = TricksTable([SUIT, SUIT, SUIT, SUIT, NT]);
    const CONTRACT: Contract = Contract::new(1, Strain::Notrump, Penalty::Undoubled);
    assert_eq!(
        Solver::lock().solve_deal(DEAL.build_full().unwrap()),
        SOLUTION
    );

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
    const DEAL: Builder = Builder::new()
        .north(Hand::new(A54, QJ32, K976, T8))
        .east(Hand::new(T8, A54, QJ32, K976))
        .south(Hand::new(K976, T8, A54, QJ32))
        .west(Hand::new(QJ32, K976, T8, A54));
    let solver = Solver::lock();
    let tricks = solver.solve_deal(DEAL.build_full().unwrap());
    let found = solver.solve_board(Objective {
        board: Board::new(Strain::Notrump, Seat::North, DEAL.build_subset().unwrap()).unwrap(),
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
    const DEAL: Builder = Builder::new()
        .north(Hand::new(A54, QJ32, K976, T8))
        .east(Hand::new(T8, A54, QJ32, K976))
        .south(Hand::new(K976, T8, A54, QJ32))
        .west(Hand::new(QJ32, K976, T8, A54));
    let solver = Solver::lock();
    let obj = Objective {
        board: Board::new(Strain::Notrump, Seat::North, DEAL.build_subset().unwrap()).unwrap(),
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
    const DEAL: Builder = Builder::new()
        .north(Hand::new(
            Holding::ALL,
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::EMPTY,
        ))
        .east(Hand::new(
            Holding::EMPTY,
            Holding::ALL,
            Holding::EMPTY,
            Holding::EMPTY,
        ))
        .south(Hand::new(
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::ALL,
            Holding::EMPTY,
        ))
        .west(Hand::new(
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::ALL,
        ));
    let solver = Solver::lock();
    let expected = solver.solve_deal(DEAL.build_full().unwrap());

    let deals = [DEAL.build_full().unwrap(); 41];
    let tables = solver.solve_deals(&deals, NonEmptyStrainFlags::ALL);
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
    const DEAL: Builder = Builder::new()
        .north(Hand::new(A54, QJ32, K976, T8))
        .east(Hand::new(T8, A54, QJ32, K976))
        .south(Hand::new(K976, T8, A54, QJ32))
        .west(Hand::new(QJ32, K976, T8, A54));
    let board = Board::new(Strain::Notrump, Seat::North, DEAL.build_subset().unwrap()).unwrap();
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
    const DEAL: Builder = Builder::new()
        .north(Hand::new(A54, QJ32, K976, T8))
        .east(Hand::new(T8, A54, QJ32, K976))
        .south(Hand::new(K976, T8, A54, QJ32))
        .west(Hand::new(QJ32, K976, T8, A54));
    let board = Board::new(Strain::Notrump, Seat::North, DEAL.build_subset().unwrap()).unwrap();
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
    const DEAL: Builder = Builder::new()
        .north(Hand::new(
            Holding::ALL,
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::EMPTY,
        ))
        .east(Hand::new(
            Holding::EMPTY,
            Holding::ALL,
            Holding::EMPTY,
            Holding::EMPTY,
        ))
        .south(Hand::new(
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::ALL,
            Holding::EMPTY,
        ))
        .west(Hand::new(
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::EMPTY,
            Holding::ALL,
        ));
    let mut cards = ArrayVec::<Card, 52>::new();
    cards.push(Card {
        suit: Suit::Clubs,
        rank: Rank::A,
    });
    let analysis = Solver::lock().analyse_play(PlayTrace {
        board: Board::new(Strain::Notrump, Seat::North, DEAL.build_subset().unwrap()).unwrap(),
        cards,
    });
    assert_eq!(analysis.tricks.len(), 2);
    assert!(analysis.tricks.iter().all(|&t| t == 0));
}

#[test]
fn system_info_version_is_2_9_0() {
    let info = Solver::lock().system_info();
    assert_eq!(info.version(), Version::new(2, 9, 0));
}

#[test]
fn system_info_platform_matches_os() {
    let platform = match () {
        () if cfg!(target_os = "linux") => Platform::Linux,
        () if cfg!(target_os = "macos") => Platform::Apple,
        () if cfg!(target_os = "cygwin") => Platform::Cygwin,
        () if cfg!(target_os = "windows") => Platform::Windows,
        () => return, // Skip test on unknown platforms
    };
    let info = Solver::lock().system_info();
    assert_eq!(info.platform(), platform);
}

#[test]
fn system_info_num_bits_matches_target() {
    let num_bits: u32 = match () {
        () if cfg!(target_pointer_width = "64") => 64,
        () if cfg!(target_pointer_width = "32") => 32,
        () if cfg!(target_pointer_width = "16") => 16,
        () => return, // Skip test on unknown pointer widths
    };
    let info = Solver::lock().system_info();
    assert_eq!(info.num_bits(), num_bits);
}

#[test]
fn system_info_compiler_is_known() {
    let info = Solver::lock().system_info();
    assert!(!matches!(info.compiler(), Compiler::Unknown(_)));
}

#[test]
fn system_info_threading_is_stl() {
    let info = Solver::lock().system_info();
    assert_eq!(info.threading(), Threading::STL);
}

#[test]
fn system_info_num_cores_is_positive() {
    let info = Solver::lock().system_info();
    assert!(info.num_cores() > 0);
}

#[test]
fn system_info_num_threads_is_positive() {
    let info = Solver::lock().system_info();
    assert!(info.num_threads() > 0);
}

#[test]
fn system_info_thread_sizes_is_nonempty() {
    let info = Solver::lock().system_info();
    assert!(!info.thread_sizes().is_empty());
}

#[test]
fn system_info_system_string_is_nonempty() {
    let info = Solver::lock().system_info();
    assert!(!info.system_string().is_empty());
}

#[test]
fn system_info_display_matches_system_string() {
    let info = Solver::lock().system_info();
    assert_eq!(info.to_string(), info.system_string());
}

#[test]
fn tricks_row_try_new_rejects_out_of_range() {
    assert_eq!(TricksRow::try_new(14, 0, 0, 0), Err(InvalidTricks));
    assert_eq!(TricksRow::try_new(0, 14, 0, 0), Err(InvalidTricks));
    assert_eq!(TricksRow::try_new(0, 0, 14, 0), Err(InvalidTricks));
    assert_eq!(TricksRow::try_new(0, 0, 0, 14), Err(InvalidTricks));
    assert!(TricksRow::try_new(13, 13, 13, 13).is_ok());
    assert!(TricksRow::try_new(0, 0, 0, 0).is_ok());
}
