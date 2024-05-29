use super::Contract;
use super::Penalty::*;
use super::Strain::*;

macro_rules! static_assert {
    ($cond:expr) => {
        const _: () = [()][!$cond as usize];
    };
}

static_assert!(Contract::new(1, Clubs, None).score(9, false) == 110);
static_assert!(Contract::new(1, Hearts, None).score(9, false) == 140);
static_assert!(Contract::new(1, Notrump, None).score(7, false) == 90);

static_assert!(Contract::new(3, Notrump, None).score(9, false) == 400);
static_assert!(Contract::new(3, Notrump, None).score(9, true) == 600);
static_assert!(Contract::new(4, Hearts, None).score(10, false) == 420);
static_assert!(Contract::new(4, Spades, None).score(10, true) == 620);
static_assert!(Contract::new(5, Clubs, None).score(11, false) == 400);
static_assert!(Contract::new(5, Diamonds, None).score(11, true) == 600);

static_assert!(Contract::new(6, Spades, None).score(12, true) == 1430);
static_assert!(Contract::new(6, Notrump, None).score(12, false) == 990);

static_assert!(Contract::new(2, Clubs, Doubled).score(8, false) == 180);
static_assert!(Contract::new(2, Clubs, Doubled).score(9, false) == 280);
static_assert!(Contract::new(2, Clubs, Doubled).score(9, true) == 380);

static_assert!(Contract::new(1, Notrump, Redoubled).score(8, true) == 1160);
static_assert!(Contract::new(7, Spades, Redoubled).score(13, false) == 2240);

const _: () = {
    const fn test_set_contract(bid: super::Bid) {
        let undoubled = Contract::new(bid.level, bid.strain, None);
        let mut tricks = 0;

        while tricks < bid.level + 6 {
            let undertricks = (bid.level + 6 - tricks) as i32;
            assert!(undoubled.score(tricks, false) == -50 * undertricks);
            assert!(undoubled.score(tricks, true) == -100 * undertricks);
            tricks += 1;
        }

        let doubled = Contract::new(bid.level, bid.strain, Doubled);

        assert!(doubled.score(bid.level + 5, false) == -100);
        assert!(doubled.score(bid.level + 4, false) == -300);
        assert!(doubled.score(bid.level + 3, false) == -500);
        assert!(doubled.score(bid.level + 2, false) == -800);

        assert!(doubled.score(bid.level + 5, true) == -200);
        assert!(doubled.score(bid.level + 4, true) == -500);
        assert!(doubled.score(bid.level + 3, true) == -800);
        assert!(doubled.score(bid.level + 2, true) == -1100);

        let redoubled = Contract::new(bid.level, bid.strain, Redoubled);
        let mut tricks = 0;

        while tricks < bid.level + 6 {
            assert!(redoubled.score(tricks, false) == 2 * doubled.score(tricks, false));
            assert!(redoubled.score(tricks, true) == 2 * doubled.score(tricks, true));
            tricks += 1;
        }
    }

    let mut level = 1;

    while level <= 7 {
        test_set_contract(super::Bid::new(level, Clubs));
        test_set_contract(super::Bid::new(level, Diamonds));
        test_set_contract(super::Bid::new(level, Hearts));
        test_set_contract(super::Bid::new(level, Spades));
        test_set_contract(super::Bid::new(level, Notrump));
        level += 1;
    }
};
