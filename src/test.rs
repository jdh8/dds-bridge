#![cfg(test)]
use rand::prelude::SliceRandom as _;
use crate::contract::Strain;
use crate::deal::{Card, Deal, SmallSet as _};

#[derive(Clone, Debug, Default)]
pub struct Deck {
    pub cards: Vec<Card>,
}

impl Deck {
    pub fn standard_52() -> Self {
        let suits = [
            Strain::Clubs,
            Strain::Diamonds,
            Strain::Hearts,
            Strain::Spades,
        ];
        let product = suits.iter().flat_map(|x| core::iter::repeat(x).zip(2..15));
        Self {
            cards: product.map(|(suit, rank)| Card::new(*suit, rank)).collect(),
        }
    }

    pub fn deal(&self) -> Deal {
        let mut deal = Deal::default();

        for (index, card) in self.cards.iter().enumerate() {
            #[allow(clippy::cast_possible_truncation)]
            deal[unsafe { core::mem::transmute((index & 0x3) as u8) }].insert(*card);
        }

        deal
    }

    pub fn shuffle(&mut self) {
        self.cards.shuffle(&mut rand::thread_rng());
    }
}

pub fn shuffled_standard_52_deck() -> Deck {
    let mut deck = Deck::standard_52();
    deck.shuffle();
    deck
}
