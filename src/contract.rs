use crate::Strain;
use core::fmt::{self, Write as _};
use core::num::NonZero;
use thiserror::Error;

/// Error indicating an invalid level
///
/// The level of a contract must be in `1..=7`
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[error("{0} is not a valid level (1..=7)")]
pub struct InvalidLevel(u8);

/// The level of a contract, from 1 to 7
///
/// The number of tricks (adding the book of 6 tricks) to take to fulfill
/// the contract
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Level(NonZero<u8>);

impl Level {
    /// Create a level from a number of tricks
    ///
    /// # Panics
    ///
    /// When the level is not in `1..=7`.  In const contexts, this is a
    /// compile-time error.
    #[must_use]
    #[inline]
    pub const fn new(level: u8) -> Self {
        match Self::try_new(level) {
            Ok(l) => l,
            Err(_) => panic!("level must be in 1..=7"),
        }
    }

    /// Try to create a level from a number of tricks
    ///
    /// # Errors
    ///
    /// When the level is not in `1..=7`.
    #[inline]
    pub const fn try_new(level: u8) -> Result<Self, InvalidLevel> {
        match NonZero::new(level) {
            Some(nonzero) if level <= 7 => Ok(Self(nonzero)),
            _ => Err(InvalidLevel(level)),
        }
    }

    /// Get the stored level as [`u8`]
    #[must_use]
    #[inline]
    pub const fn get(self) -> u8 {
        self.0.get()
    }
}

/// A call that proposes a contract
///
/// The order of the fields ensures natural ordering by deriving [`PartialOrd`]
/// and [`Ord`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bid {
    /// The level of the contract
    pub level: Level,

    /// The strain of the contract
    pub strain: Strain,
}

impl fmt::Display for Bid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.level.get(), self.strain)
    }
}

/// Penalty inflicted on a contract
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Penalty {
    /// No penalty
    Undoubled,
    /// Penalty by doubling
    Doubled,
    /// Penalty by redoubling
    Redoubled,
}

impl fmt::Display for Penalty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undoubled => Ok(()),
            Self::Doubled => f.write_char('x'),
            Self::Redoubled => f.write_str("xx"),
        }
    }
}

/// The statement of the pair winning the bidding that they will take at least
/// the number of tricks (in addition to the book of 6 tricks), and the strain
/// denotes the trump suit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Contract {
    /// The basic part of a contract
    pub bid: Bid,
    /// The penalty inflicted on the contract
    pub penalty: Penalty,
}

impl fmt::Display for Contract {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.bid, self.penalty)
    }
}

impl From<Bid> for Contract {
    #[inline]
    fn from(bid: Bid) -> Self {
        Self {
            bid,
            penalty: Penalty::Undoubled,
        }
    }
}

#[inline]
const fn compute_doubled_penalty(undertricks: i32, vulnerable: bool) -> i32 {
    match undertricks + vulnerable as i32 {
        1 => 100,
        2 => {
            if vulnerable {
                200
            } else {
                300
            }
        }
        many => 300 * many - 400,
    }
}

impl Contract {
    /// Base score for making this contract
    ///
    /// <https://en.wikipedia.org/wiki/Bridge_scoring#Contract_points>
    #[must_use]
    #[inline]
    pub const fn contract_points(self) -> i32 {
        let level = self.bid.level.get() as i32;
        let per_trick = self.bid.strain.is_minor() as i32 * -10 + 30;
        let notrump = self.bid.strain.is_notrump() as i32 * 10;
        (per_trick * level + notrump) << (self.penalty as u8)
    }

    /// Score for this contract given the number of taken tricks and
    /// vulnerability
    ///
    /// The `vulnerable` parameter refers to the *declaring* side's
    /// vulnerability, not the defenders'.
    ///
    /// The score is positive if the declarer makes the contract, and negative
    /// if the declarer fails.
    #[must_use]
    #[inline]
    pub const fn score(self, tricks: u8, vulnerable: bool) -> i32 {
        let overtricks = tricks as i32 - self.bid.level.get() as i32 - 6;

        if overtricks >= 0 {
            let base = self.contract_points();
            let game = if base < 100 {
                50
            } else if vulnerable {
                500
            } else {
                300
            };
            let doubled = self.penalty as i32 * 50;

            let slam = match self.bid.level.get() {
                6 => (vulnerable as i32 + 2) * 250,
                7 => (vulnerable as i32 + 2) * 500,
                _ => 0,
            };

            let per_trick = match self.penalty {
                Penalty::Undoubled => self.bid.strain.is_minor() as i32 * -10 + 30,
                penalty => penalty as i32 * if vulnerable { 200 } else { 100 },
            };

            base + game + slam + doubled + overtricks * per_trick
        } else {
            match self.penalty {
                Penalty::Undoubled => overtricks * if vulnerable { 100 } else { 50 },
                penalty => penalty as i32 * -compute_doubled_penalty(-overtricks, vulnerable),
            }
        }
    }
}
