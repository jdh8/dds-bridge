#[cfg(test)]
mod test;

/// Denomination, a suit or notrump
///
/// We choose this representation over `Option<Suit>` because we are not sure if
/// the latter can be optimized to a single byte.
///
/// The order of the suits deviates from [`dds`][dds], but this order provides
/// natural ordering by deriving [`PartialOrd`] and [`Ord`].
///
/// [dds]: https://github.com/dds-bridge/dds
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Strain {
    /// ♣
    Clubs,
    /// ♦
    Diamonds,
    /// ♥
    Hearts,
    /// ♠
    Spades,
    /// NT, the strain not proposing a trump suit
    Notrump,
}

impl Strain {
    /// Whether this strain is a minor suit (clubs or diamonds)
    #[must_use]
    pub const fn is_minor(self) -> bool {
        matches!(self, Self::Clubs | Self::Diamonds)
    }

    /// Whether this strain is a major suit (hearts or spades)
    #[must_use]
    pub const fn is_major(self) -> bool {
        matches!(self, Self::Hearts | Self::Spades)
    }

    /// Whether this strain is a suit
    #[must_use]
    pub const fn is_suit(self) -> bool {
        !matches!(self, Self::Notrump)
    }

    /// Whether this strain is notrump
    #[must_use]
    pub const fn is_notrump(self) -> bool {
        matches!(self, Self::Notrump)
    }

    /// Helper constant for iteration over all strains
    pub const ALL: [Self; 5] = [
        Self::Clubs,
        Self::Diamonds,
        Self::Hearts,
        Self::Spades,
        Self::Notrump,
    ];

    /// Helper constant for iteration over all suits
    pub const SUITS: [Self; 4] = [Self::Clubs, Self::Diamonds, Self::Hearts, Self::Spades];
}

/// A call that proposes a contract
///
/// The order of the fields ensures natural ordering by deriving [`PartialOrd`]
/// and [`Ord`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bid {
    /// The number of tricks (adding the book of 6 tricks) to take to fulfill
    /// the contract
    pub level: u8,

    /// The strain of the contract
    pub strain: Strain,
}

impl Bid {
    /// Create a bid from level and strain
    #[must_use]
    pub const fn new(level: u8, strain: Strain) -> Self {
        Self { level, strain }
    }
}

/// Any legal announcement in the bidding stage
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Call {
    /// A call indicating no wish to change the contract
    Pass,
    /// A call increasing penalties and bonuses for the contract
    Double,
    /// A call doubling the score to the previous double
    Redouble,
    /// A call proposing a contract
    Bid(Bid),
}

impl From<Bid> for Call {
    fn from(bid: Bid) -> Self {
        Self::Bid(bid)
    }
}

/// Penalty inflicted on a contract
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Penalty {
    /// No penalty
    None,
    /// Penalty by [`Call::Double`]
    Doubled,
    /// Penalty by [`Call::Redouble`]
    Redoubled,
}

/// The statement of the pair winning the bidding that they will take at least
/// the number of tricks (in addition to the book of 6 tricks), and the strain
/// denotes the trump suit.
#[derive(Debug, Clone, Copy)]
pub struct Contract {
    /// The basic part of a contract
    pub bid: Bid,
    /// The penalty inflicted on the contract
    pub penalty: Penalty,
}

impl From<Bid> for Contract {
    fn from(bid: Bid) -> Self {
        Self {
            bid,
            penalty: Penalty::None,
        }
    }
}

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
    /// Create a contract from level, strain, and penalty
    #[must_use]
    pub const fn new(level: u8, strain: Strain, penalty: Penalty) -> Self {
        Self {
            bid: Bid::new(level, strain),
            penalty,
        }
    }

    /// Base score for making this contract
    ///
    /// <https://en.wikipedia.org/wiki/Bridge_scoring#Contract_points>
    #[must_use]
    pub const fn contract_points(self) -> i32 {
        let level = self.bid.level as i32;
        let per_trick = self.bid.strain.is_minor() as i32 * -10 + 30;
        let notrump = self.bid.strain.is_notrump() as i32 * 10;
        (per_trick * level + notrump) << (self.penalty as u8)
    }

    /// Score for this contract given the number of taken tricks and
    /// vulnerability
    ///
    /// The score is positive if the declarer makes the contract, and negative
    /// if the declarer fails.
    #[must_use]
    pub const fn score(self, tricks: u8, vulnerable: bool) -> i32 {
        let overtricks = tricks as i32 - self.bid.level as i32 - 6;

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

            let slam = match self.bid.level {
                6 => (vulnerable as i32 + 2) * 250,
                7 => (vulnerable as i32 + 2) * 500,
                _ => 0,
            };

            let per_trick = match self.penalty {
                Penalty::None => self.bid.strain.is_minor() as i32 * -10 + 30,
                penalty => penalty as i32 * if vulnerable { 200 } else { 100 },
            };

            base + game + slam + doubled + overtricks * per_trick
        } else {
            match self.penalty {
                Penalty::None => overtricks * if vulnerable { 100 } else { 50 },
                penalty => penalty as i32 * -compute_doubled_penalty(-overtricks, vulnerable),
            }
        }
    }
}
