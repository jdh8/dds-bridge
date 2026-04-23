//! Play-trace input and play-analysis output types

use super::board::Board;
use super::ffi;
use super::tricks::TrickCount;
use crate::hand::{Card, Holding};

use arrayvec::ArrayVec;
use core::ffi::c_int;
use dds_bridge_sys as sys;

/// A starting board and a sequence of cards played from it
///
/// Input to [`Solver::analyse_play`](super::Solver::analyse_play).  The two
/// fields split the position and the play-trace cleanly:
///
/// - [`board`](Self::board) is the snapshot from which analysis begins.  It
///   encodes the state at the start of a trick — possibly with up to three
///   cards already on the table in [`Board::current_cards`] — and
///   [`Board::remaining`] holds only the cards still in each hand.  Cards
///   from **previously completed tricks are not represented individually**;
///   they are simply absent from `remaining`.
/// - [`cards`](Self::cards) is the play trace to replay from that snapshot,
///   in chronological order.  The first card in `cards` is whichever card
///   comes *after* any already in `board.current_cards` — it does **not**
///   restart the trick or repeat prior history.  Each card must be legal
///   (follow suit when possible and be held by the player on turn).
///
/// `cards` may span trick boundaries; DDS tracks trick completion and whose
/// lead follows internally.  The trace length may be any value from `0` to
/// `52`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PlayTrace {
    /// Snapshot at the start of analysis: state at the start of the current
    /// trick, plus any 0–3 cards already played to it via
    /// [`Board::current_cards`]
    pub board: Board,
    /// Cards played after `board`, in chronological order; may cross tricks
    pub cards: ArrayVec<Card, 52>,
}

/// Thin wrapper over [`sys::playTraceBin`] so we can impl conversions for it
#[repr(transparent)]
pub(super) struct PlayTraceBin(pub(super) sys::playTraceBin);

impl From<&ArrayVec<Card, 52>> for PlayTraceBin {
    fn from(cards: &ArrayVec<Card, 52>) -> Self {
        let mut play = sys::playTraceBin {
            // SAFETY: ArrayVec ensures the length is always in 0..=52
            #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
            number: cards.len() as c_int,
            ..Default::default()
        };

        for (i, card) in cards.iter().enumerate() {
            play.suit[i] = 3 - card.suit as c_int;
            play.rank[i] = c_int::from(card.rank.get());
        }
        Self(play)
    }
}

/// Double-dummy trick counts before and after each played card in a trace
///
/// Returned by [`Solver::analyse_play`](super::Solver::analyse_play).  Trick
/// counts are from the declarer's viewpoint: declarer is the right-hand
/// opponent of the opening leader (the side to lead the very first trick in
/// the starting [`Board`]).
///
/// `tricks[0]` is the DD value before any card in the trace is played.
/// `tricks[i]` for `i > 0` is the DD value after the i-th card.  A drop from
/// `tricks[i - 1]` to `tricks[i]` means that card was a double-dummy mistake
/// by the side to move at the time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlayAnalysis {
    /// Trick counts — `cards.len() + 1` entries, starting with the position
    /// before any card is played
    pub tricks: ArrayVec<TrickCount, 53>,
}

impl From<sys::solvedPlay> for PlayAnalysis {
    fn from(solved: sys::solvedPlay) -> Self {
        let number = ffi::count_from_sys(solved.number, solved.tricks.len());
        let mut tricks = ArrayVec::new();
        for &n in &solved.tricks[..number] {
            tricks.push(ffi::trick_count_from_sys(n));
        }
        Self { tricks }
    }
}

/// A play and its consequences
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Play {
    /// The card to play, the highest in a sequence
    ///
    /// For example, if the solution is to play a card from ♥KQJ, this field
    /// would be ♥K.
    pub card: Card,

    /// Lower equals in the sequence
    ///
    /// Playing any card in a sequence is equal in bridge and many trick-taking
    /// games.  This field contains lower cards in the sequence as `card`.  For
    /// example, if the solution is to play KQJ, this field would contain QJ.
    pub equals: Holding,

    /// Tricks this play would score
    pub score: TrickCount,
}

/// Solved plays for a board
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FoundPlays {
    /// The plays and their consequences
    pub plays: ArrayVec<Play, 13>,
    /// The number of nodes searched by the solver
    pub nodes: u32,
}

impl From<sys::futureTricks> for FoundPlays {
    fn from(future: sys::futureTricks) -> Self {
        let cards = ffi::count_from_sys(future.cards, future.suit.len());
        let mut plays = ArrayVec::new();
        for i in 0..cards {
            #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
            let equals = Holding::from_bits_truncate(future.equals[i] as u16);
            plays.push(Play {
                card: Card {
                    suit: ffi::suit_from_desc_index(future.suit[i]),
                    rank: ffi::rank_from_sys(future.rank[i]),
                },
                equals,
                score: ffi::trick_count_from_sys(future.score[i]),
            });
        }

        Self {
            plays,
            #[allow(clippy::cast_sign_loss)]
            nodes: future.nodes as u32,
        }
    }
}
