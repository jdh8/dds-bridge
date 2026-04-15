use dds_bridge::{Bid, Contract, Level, Penalty, Strain};

#[test]
fn test_level_display_roundtrip() {
    for n in 1..=7 {
        let level = Level::new(n);
        assert_eq!(level.to_string().parse(), Ok(level));
    }
}

#[test]
fn test_strain_display_roundtrip() {
    for strain in Strain::ASC {
        assert_eq!(strain.to_string().parse(), Ok(strain));
    }
}

#[test]
fn test_penalty_display_roundtrip() {
    let cases = [
        (Penalty::Undoubled, ""),
        (Penalty::Doubled, "x"),
        (Penalty::Redoubled, "xx"),
    ];
    for (penalty, s) in &cases {
        assert_eq!(penalty.to_string(), *s);
        assert_eq!(*penalty, s.parse().unwrap());
    }
}

#[test]
fn test_bid_display_roundtrip() {
    const CASES: [Bid; 5] = [
        Bid::new(Level::new(1), Strain::Clubs),
        Bid::new(Level::new(7), Strain::Notrump),
        Bid::new(Level::new(3), Strain::Hearts),
        Bid::new(Level::new(2), Strain::Spades),
        Bid::new(Level::new(5), Strain::Diamonds),
    ];

    for bid in CASES {
        assert_eq!(bid.to_string().parse(), Ok(bid));
    }
}

#[test]
fn test_bid_fromstr_alternates() {
    // Test hollow suit symbols and letters
    let cases = [
        ("1C", Bid::new(Level::new(1), Strain::Clubs)),
        ("1♣", Bid::new(Level::new(1), Strain::Clubs)),
        ("2D", Bid::new(Level::new(2), Strain::Diamonds)),
        ("2♦", Bid::new(Level::new(2), Strain::Diamonds)),
        ("3H", Bid::new(Level::new(3), Strain::Hearts)),
        ("3♥", Bid::new(Level::new(3), Strain::Hearts)),
        ("4S", Bid::new(Level::new(4), Strain::Spades)),
        ("4♠", Bid::new(Level::new(4), Strain::Spades)),
        ("5N", Bid::new(Level::new(5), Strain::Notrump)),
        ("6NT", Bid::new(Level::new(6), Strain::Notrump)),
    ];
    for (s, bid) in &cases {
        assert_eq!(s.parse::<Bid>().unwrap(), *bid);
    }
}

#[test]
fn test_penalty_fromstr_invalid() {
    for s in ["xxx", "X", "Xx", "xX", "XxX", " ", "y"] {
        assert!(s.parse::<Penalty>().is_err());
    }
}

#[test]
fn test_contract_display_roundtrip() {
    let cases = [
        Contract {
            bid: Bid::new(Level::new(1), Strain::Clubs),
            penalty: Penalty::Undoubled,
        },
        Contract {
            bid: Bid::new(Level::new(7), Strain::Notrump),
            penalty: Penalty::Redoubled,
        },
        Contract {
            bid: Bid::new(Level::new(3), Strain::Hearts),
            penalty: Penalty::Doubled,
        },
        Contract {
            bid: Bid::new(Level::new(2), Strain::Spades),
            penalty: Penalty::Undoubled,
        },
        Contract {
            bid: Bid::new(Level::new(5), Strain::Diamonds),
            penalty: Penalty::Doubled,
        },
    ];
    for contract in &cases {
        let display = contract.to_string();
        assert_eq!(display.parse::<Contract>().unwrap(), *contract);
    }
}

#[test]
fn test_contract_fromstr_alternates() {
    let cases = [
        (
            "1C",
            Contract {
                bid: Bid::new(Level::new(1), Strain::Clubs),
                penalty: Penalty::Undoubled,
            },
        ),
        (
            "1Cx",
            Contract {
                bid: Bid::new(Level::new(1), Strain::Clubs),
                penalty: Penalty::Doubled,
            },
        ),
        (
            "1Cxx",
            Contract {
                bid: Bid::new(Level::new(1), Strain::Clubs),
                penalty: Penalty::Redoubled,
            },
        ),
        (
            "1♣x",
            Contract {
                bid: Bid::new(Level::new(1), Strain::Clubs),
                penalty: Penalty::Doubled,
            },
        ),
        (
            "2NT",
            Contract {
                bid: Bid::new(Level::new(2), Strain::Notrump),
                penalty: Penalty::Undoubled,
            },
        ),
        (
            "2NTx",
            Contract {
                bid: Bid::new(Level::new(2), Strain::Notrump),
                penalty: Penalty::Doubled,
            },
        ),
        (
            "3♥xx",
            Contract {
                bid: Bid::new(Level::new(3), Strain::Hearts),
                penalty: Penalty::Redoubled,
            },
        ),
    ];
    for (s, contract) in &cases {
        assert_eq!(s.parse::<Contract>().unwrap(), *contract);
    }
}

#[test]
fn test_penalty_display_and_fromstr() {
    let cases = [
        ("", Penalty::Undoubled),
        ("x", Penalty::Doubled),
        ("xx", Penalty::Redoubled),
    ];
    for (s, penalty) in &cases {
        assert_eq!(s, &penalty.to_string());
        assert_eq!(s.parse::<Penalty>().unwrap(), *penalty);
    }
}
