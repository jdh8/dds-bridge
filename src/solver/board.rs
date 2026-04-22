//! Solving input: boards, tricks-in-progress, targets, and objectives

use crate::deal::PartialDeal;
use crate::hand::{Card, Hand};
use crate::seat::Seat;
use crate::{Strain, Suit};

use arrayvec::ArrayVec;
use dds_bridge_sys as sys;
use thiserror::Error;

use core::ffi::c_int;

/// Target tricks and number of solutions to find
///
/// This enum corresponds to a tuple of `target` and `solutions` in
/// [`sys::SolveBoard`].  The `target` tricks given as an associated value must
/// be in the range of `-1..=13`, where `-1` instructs the solver to find cards
/// that give the most tricks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Target {
    /// Find any card that fulfills the target
    ///
    /// - `0..=13`: Find any card scoring at least `target` tricks
    /// - `-1`: Find any card scoring the most tricks
    Any(i8),

    /// Find all cards that fulfill the target
    ///
    /// - `0..=13`: Find all cards scoring at least `target` tricks
    /// - `-1`: Find all cards scoring the most tricks
    All(i8),

    /// Solve for all legal plays
    ///
    /// Cards are sorted with their scores in descending order.
    Legal,
}

impl Target {
    /// Get the `target` argument for [`sys::SolveBoard`]
    #[must_use]
    #[inline]
    pub const fn target(self) -> c_int {
        match self {
            Self::Any(target) | Self::All(target) => target as c_int,
            Self::Legal => -1,
        }
    }

    /// Get the `solutions` argument for [`sys::SolveBoard`]
    #[must_use]
    #[inline]
    pub const fn solutions(self) -> c_int {
        match self {
            Self::Any(_) => 1,
            Self::All(_) => 2,
            Self::Legal => 3,
        }
    }
}

/// Error returned when constructing a [`Board`] with invalid invariants
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum BoardError {
    /// A card on the table is still present in one of the remaining hands
    #[error("A played card is also present in a remaining hand")]
    PlayedCardInHand,
    /// The remaining hand sizes do not match the number of played cards
    ///
    /// With `k` cards on the table, exactly the `k` seats starting from
    /// `leader` (in playing order) must have one fewer card than the other
    /// seats; all other seats must share a common size.
    #[error(
        "Remaining hand sizes do not match the played-count pattern \
         (the k seats from leader must have size m-1; others m)"
    )]
    InconsistentHandSizes,
    /// A played card does not follow suit though the player held the led suit
    ///
    /// `index` is the position within the current trick of the offending card
    /// (always ≥ 1, since the lead itself cannot revoke).
    #[error("Played card at index {index} is a revoke — player held the led suit")]
    Revoke {
        /// Position of the revoking card within the current trick
        index: u8,
    },
}

/// Error returned when pushing cards to a [`CurrentTrick`]
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum CurrentTrickError {
    /// More than three cards are on the table
    #[error("A trick can hold at most 3 cards on the table before it completes")]
    TooManyPlayed,
    /// The same card appears twice among the played cards
    #[error("Duplicate card in the played cards on the table")]
    DuplicatePlayedCard,
}

/// Trick-in-progress — 0 to 3 cards played, in playing order
///
/// Cards are played by the seats starting at [`leader`](Self::leader) in playing
/// order: the first card by `leader`, the second by `leader.lho()`, and so on.
///
/// # Invariants
///
/// 1. At most 3 cards are stored (enforced by the backing `ArrayVec<Card, 3>`).
/// 2. The stored cards are pairwise distinct.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CurrentTrick {
    trump: Strain,
    leader: Seat,
    cards: ArrayVec<Card, 3>,
    seen: Hand,
}

impl CurrentTrick {
    /// Empty trick led by `leader` under `trump`
    #[must_use]
    #[inline]
    pub const fn new(trump: Strain, leader: Seat) -> Self {
        Self {
            trump,
            leader,
            cards: ArrayVec::new_const(),
            seen: Hand::EMPTY,
        }
    }

    /// Build from a slice, validating the 0–3-card length and pairwise
    /// disjointness invariants.
    ///
    /// # Errors
    ///
    /// Returns a [`CurrentTrickError`] if the slice has more than 3 entries or
    /// contains a duplicate card.
    pub fn from_slice(
        trump: Strain,
        leader: Seat,
        played: &[Card],
    ) -> Result<Self, CurrentTrickError> {
        let mut trick = Self::new(trump, leader);
        for &card in played {
            trick.try_push(card)?;
        }
        Ok(trick)
    }

    /// Append one card to the trick.
    ///
    /// # Errors
    ///
    /// Returns [`CurrentTrickError::TooManyPlayed`] if the trick already holds
    /// 3 cards, or [`CurrentTrickError::DuplicatePlayedCard`] if `card` is
    /// already in the trick.
    pub fn try_push(&mut self, card: Card) -> Result<(), CurrentTrickError> {
        if self.cards.is_full() {
            return Err(CurrentTrickError::TooManyPlayed);
        }
        if !self.seen.insert(card) {
            return Err(CurrentTrickError::DuplicatePlayedCard);
        }
        self.cards.push(card);
        Ok(())
    }

    /// Strain of the contract governing this trick
    #[must_use]
    #[inline]
    pub const fn trump(&self) -> Strain {
        self.trump
    }

    /// Seat that led this trick
    #[must_use]
    #[inline]
    pub const fn leader(&self) -> Seat {
        self.leader
    }

    /// Cards played so far, in playing order
    #[must_use]
    #[inline]
    pub fn cards(&self) -> &[Card] {
        &self.cards
    }

    /// Number of cards played so far (0 to 3)
    #[must_use]
    #[inline]
    pub const fn len(&self) -> usize {
        self.cards.len()
    }

    /// Whether no cards have been played yet
    #[must_use]
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.cards.is_empty()
    }

    /// Bitmask union of the cards played so far
    #[must_use]
    #[inline]
    pub const fn seen(&self) -> Hand {
        self.seen
    }

    /// Suit led this trick, or `None` if no card has been played yet
    #[must_use]
    #[inline]
    pub fn led_suit(&self) -> Option<Suit> {
        self.cards.first().map(|c| c.suit)
    }
}

/// A snapshot of a board
///
/// Construct via [`Board::try_new`], which handles both start-of-trick
/// (use [`CurrentTrick::new`]) and mid-trick (0–3 played cards) cases.  The
/// invariants below are enforced by the constructor.
///
/// # Invariants
///
/// 1. `remaining` is a valid [`PartialDeal`] (≤13 cards per hand, pairwise
///    disjoint).
/// 2. Each card in the current trick is absent from every remaining hand (the
///    "already played" invariant).
/// 3. **Uniform-size-after-restoration**: putting the
///    `k = current_trick.len()` table cards back into their players' hands
///    yields a subset where all four hands share a common size `m`.
///    Equivalently, the `k` seats starting at `current_trick.leader()` (in
///    playing order: `leader`, `leader.lho()`, …) have size `m − 1` and the
///    remaining `4 − k` seats have size `m`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Board {
    current_trick: CurrentTrick,
    remaining: PartialDeal,
}

impl Board {
    /// Construct a mid-trick board from a pre-validated [`CurrentTrick`] and
    /// the cards remaining in each hand.
    ///
    /// # Errors
    ///
    /// Returns a [`BoardError`] if the invariants documented on [`Board`] do
    /// not hold.
    pub fn try_new(
        remaining: PartialDeal,
        current_trick: CurrentTrick,
    ) -> Result<Self, BoardError> {
        if !(current_trick.seen() & remaining.collected()).is_empty() {
            return Err(BoardError::PlayedCardInHand);
        }

        let leader = current_trick.leader();
        let seats = [leader, leader.lho(), leader.partner(), leader.rho()];
        let index = current_trick.len();
        // Leader's RHO has not yet played this trick, so its hand length is the
        // common "full" length we expect.
        let full_len = remaining[leader.rho()].len();
        for (j, &seat) in seats.iter().enumerate() {
            if remaining[seat].len() + usize::from(j < index) != full_len {
                return Err(BoardError::InconsistentHandSizes);
            }
        }

        if let Some(led_suit) = current_trick.led_suit() {
            for (j, played_card) in current_trick.cards().iter().enumerate().skip(1) {
                if played_card.suit != led_suit && !remaining[seats[j]][led_suit].is_empty() {
                    return Err(BoardError::Revoke {
                        // `j < 3` constrained by `ArrayVec<Card, 3>`
                        #[allow(clippy::cast_possible_truncation)]
                        index: j as u8,
                    });
                }
            }
        }

        Ok(Self {
            current_trick,
            remaining,
        })
    }

    /// Strain of the contract
    #[must_use]
    #[inline]
    pub const fn trump(&self) -> Strain {
        self.current_trick.trump()
    }

    /// Seat leading the current trick
    #[must_use]
    #[inline]
    pub const fn leader(&self) -> Seat {
        self.current_trick.leader()
    }

    /// Cards already played to the current trick, in playing order
    #[must_use]
    #[inline]
    pub fn current_cards(&self) -> &[Card] {
        self.current_trick.cards()
    }

    /// The current trick — cards played so far plus trump and leader
    #[must_use]
    #[inline]
    pub const fn current_trick(&self) -> &CurrentTrick {
        &self.current_trick
    }

    /// Remaining cards in each hand
    #[must_use]
    #[inline]
    pub const fn remaining(&self) -> &PartialDeal {
        &self.remaining
    }
}

impl From<Board> for sys::deal {
    fn from(board: Board) -> Self {
        let mut suits = [0; 3];
        let mut ranks = [0; 3];

        for (i, card) in board.current_trick.cards().iter().enumerate() {
            suits[i] = 3 - card.suit as c_int;
            ranks[i] = c_int::from(card.rank.get());
        }

        Self {
            trump: match board.current_trick.trump() {
                Strain::Spades => 0,
                Strain::Hearts => 1,
                Strain::Diamonds => 2,
                Strain::Clubs => 3,
                Strain::Notrump => 4,
            },
            first: board.current_trick.leader() as c_int,
            currentTrickSuit: suits,
            currentTrickRank: ranks,
            remainCards: sys::ddTableDeal::from(board.remaining).cards,
        }
    }
}

/// A board and its solving target
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Objective {
    /// The board to solve
    pub board: Board,
    /// The target tricks and number of solutions to find
    pub target: Target,
}
