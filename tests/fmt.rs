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
        Bid {
            level: Level::new(1),
            strain: Strain::Clubs,
        },
        Bid {
            level: Level::new(7),
            strain: Strain::Notrump,
        },
        Bid {
            level: Level::new(3),
            strain: Strain::Hearts,
        },
        Bid {
            level: Level::new(2),
            strain: Strain::Spades,
        },
        Bid {
            level: Level::new(5),
            strain: Strain::Diamonds,
        },
    ];

    for bid in CASES {
        assert_eq!(bid.to_string().parse(), Ok(bid));
    }
}

#[test]
fn test_bid_fromstr_alternates() {
    // Test hollow suit symbols and letters
    let cases = [
        (
            "1C",
            Bid {
                level: Level::new(1),
                strain: Strain::Clubs,
            },
        ),
        (
            "1♧",
            Bid {
                level: Level::new(1),
                strain: Strain::Clubs,
            },
        ),
        (
            "2D",
            Bid {
                level: Level::new(2),
                strain: Strain::Diamonds,
            },
        ),
        (
            "2♢",
            Bid {
                level: Level::new(2),
                strain: Strain::Diamonds,
            },
        ),
        (
            "3H",
            Bid {
                level: Level::new(3),
                strain: Strain::Hearts,
            },
        ),
        (
            "3♡",
            Bid {
                level: Level::new(3),
                strain: Strain::Hearts,
            },
        ),
        (
            "4S",
            Bid {
                level: Level::new(4),
                strain: Strain::Spades,
            },
        ),
        (
            "4♤",
            Bid {
                level: Level::new(4),
                strain: Strain::Spades,
            },
        ),
        (
            "5N",
            Bid {
                level: Level::new(5),
                strain: Strain::Notrump,
            },
        ),
        (
            "6NT",
            Bid {
                level: Level::new(6),
                strain: Strain::Notrump,
            },
        ),
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
            bid: Bid {
                level: Level::new(1),
                strain: Strain::Clubs,
            },
            penalty: Penalty::Undoubled,
        },
        Contract {
            bid: Bid {
                level: Level::new(7),
                strain: Strain::Notrump,
            },
            penalty: Penalty::Redoubled,
        },
        Contract {
            bid: Bid {
                level: Level::new(3),
                strain: Strain::Hearts,
            },
            penalty: Penalty::Doubled,
        },
        Contract {
            bid: Bid {
                level: Level::new(2),
                strain: Strain::Spades,
            },
            penalty: Penalty::Undoubled,
        },
        Contract {
            bid: Bid {
                level: Level::new(5),
                strain: Strain::Diamonds,
            },
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
                bid: Bid {
                    level: Level::new(1),
                    strain: Strain::Clubs,
                },
                penalty: Penalty::Undoubled,
            },
        ),
        (
            "1Cx",
            Contract {
                bid: Bid {
                    level: Level::new(1),
                    strain: Strain::Clubs,
                },
                penalty: Penalty::Doubled,
            },
        ),
        (
            "1Cxx",
            Contract {
                bid: Bid {
                    level: Level::new(1),
                    strain: Strain::Clubs,
                },
                penalty: Penalty::Redoubled,
            },
        ),
        (
            "1♧x",
            Contract {
                bid: Bid {
                    level: Level::new(1),
                    strain: Strain::Clubs,
                },
                penalty: Penalty::Doubled,
            },
        ),
        (
            "2NT",
            Contract {
                bid: Bid {
                    level: Level::new(2),
                    strain: Strain::Notrump,
                },
                penalty: Penalty::Undoubled,
            },
        ),
        (
            "2NTx",
            Contract {
                bid: Bid {
                    level: Level::new(2),
                    strain: Strain::Notrump,
                },
                penalty: Penalty::Doubled,
            },
        ),
        (
            "3♡xx",
            Contract {
                bid: Bid {
                    level: Level::new(3),
                    strain: Strain::Hearts,
                },
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
