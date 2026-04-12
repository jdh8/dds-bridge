use dds_bridge::{Bid, Contract, Penalty, Strain};

const fn bid(level: u8, strain: Strain) -> Bid {
    match Bid::new(level, strain) {
        Ok(b) => b,
        Err(_) => panic!("Invalid bid level"),
    }
}

const fn contract(level: u8, strain: Strain, penalty: Penalty) -> Contract {
    Contract {
        bid: bid(level, strain),
        penalty,
    }
}

const C: Strain = Strain::Clubs;
const D: Strain = Strain::Diamonds;
const H: Strain = Strain::Hearts;
const S: Strain = Strain::Spades;
const N: Strain = Strain::Notrump;

const P: Penalty = Penalty::Undoubled;
const X: Penalty = Penalty::Doubled;
const XX: Penalty = Penalty::Redoubled;

const _: () = assert!(contract(1, C, P).score(9, false) == 110);
const _: () = assert!(contract(1, H, P).score(9, false) == 140);
const _: () = assert!(contract(1, N, P).score(7, false) == 90);

const _: () = assert!(contract(3, N, P).score(9, false) == 400);
const _: () = assert!(contract(3, N, P).score(9, true) == 600);
const _: () = assert!(contract(4, H, P).score(10, false) == 420);
const _: () = assert!(contract(4, S, P).score(10, true) == 620);
const _: () = assert!(contract(5, C, P).score(11, false) == 400);
const _: () = assert!(contract(5, D, P).score(11, true) == 600);

const _: () = assert!(contract(6, S, P).score(12, true) == 1430);
const _: () = assert!(contract(6, N, P).score(12, false) == 990);

const _: () = assert!(contract(2, C, X).score(8, false) == 180);
const _: () = assert!(contract(2, C, X).score(9, false) == 280);
const _: () = assert!(contract(2, C, X).score(9, true) == 380);

const _: () = assert!(contract(1, N, XX).score(8, true) == 1160);
const _: () = assert!(contract(7, S, XX).score(13, false) == 2240);

const _: () = {
    const fn test_set_contract(bid: Bid) {
        let undoubled = Contract {
            bid,
            penalty: Penalty::Undoubled,
        };
        let mut tricks = 0;

        while tricks < bid.level.get() + 6 {
            let undertricks = (bid.level.get() + 6 - tricks) as i32;
            assert!(undoubled.score(tricks, false) == -50 * undertricks);
            assert!(undoubled.score(tricks, true) == -100 * undertricks);
            tricks += 1;
        }

        let doubled = Contract {
            bid,
            penalty: Penalty::Doubled,
        };

        assert!(doubled.score(bid.level.get() + 5, false) == -100);
        assert!(doubled.score(bid.level.get() + 4, false) == -300);
        assert!(doubled.score(bid.level.get() + 3, false) == -500);
        assert!(doubled.score(bid.level.get() + 2, false) == -800);

        assert!(doubled.score(bid.level.get() + 5, true) == -200);
        assert!(doubled.score(bid.level.get() + 4, true) == -500);
        assert!(doubled.score(bid.level.get() + 3, true) == -800);
        assert!(doubled.score(bid.level.get() + 2, true) == -1100);

        let redoubled = Contract {
            bid,
            penalty: Penalty::Redoubled,
        };
        let mut tricks = 0;

        while tricks < bid.level.get() + 6 {
            assert!(redoubled.score(tricks, false) == 2 * doubled.score(tricks, false));
            assert!(redoubled.score(tricks, true) == 2 * doubled.score(tricks, true));
            tricks += 1;
        }
    }

    let mut level = 1;

    while level <= 7 {
        test_set_contract(bid(level, C));
        test_set_contract(bid(level, D));
        test_set_contract(bid(level, H));
        test_set_contract(bid(level, S));
        test_set_contract(bid(level, N));
        level += 1;
    }
};
