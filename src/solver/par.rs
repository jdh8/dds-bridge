//! Par-contract results and their conversion from DDS FFI types

use crate::Strain;
use crate::contract::{Contract, Penalty};
use crate::seat::Seat;

use dds_bridge_sys as sys;

use core::ops::BitOr as _;

/// Par contract
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ParContract {
    /// The contract
    pub contract: Contract,

    /// The declarer of the contract
    pub declarer: Seat,

    /// The number of overtricks (negative for undertricks)
    pub overtricks: i8,
}

/// Par score and contracts
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Par {
    /// The par score
    pub score: i32,

    /// The contracts that achieve the par score
    pub contracts: Vec<ParContract>,
}

impl Par {
    /// Check if two pars are equivalent
    ///
    /// Two pars are equivalent if they have the same par score and the same
    /// set of (strain, declarer) pairs.  Overtricks and duplicate entries are
    /// ignored.
    ///
    /// This is intentionally looser than [`PartialEq`], which compares every
    /// field exactly.  `equivalent` exists because DDS may report the same
    /// par result with different overtrick counts or orderings depending on
    /// the code path (e.g. `DealerParBin` vs `SidesParBin`).  Use `==` when
    /// you need exact structural equality; use `equivalent` when you only
    /// care about the strategic meaning of the par result.
    #[must_use]
    pub fn equivalent(&self, other: &Self) -> bool {
        // Since every contract scores the same, we can compare only the set of
        // (`Strain`, `Seat`).  #`Strain` * #`Seat` = 5 * 4 = 20, which fits
        // in a `u32` as a bitset.
        fn key(contracts: &[ParContract]) -> u32 {
            contracts
                .iter()
                .map(|p| 1 << ((p.contract.bid.strain as u8) << 2 | p.declarer as u8))
                .fold(0, u32::bitor)
        }
        self.score == other.score && key(&self.contracts) == key(&other.contracts)
    }
}

impl From<sys::parResultsMaster> for Par {
    fn from(par: sys::parResultsMaster) -> Self {
        // DDS returns a zero contract for par-zero deals, but we want to filter
        // it out for consistency.
        #[allow(clippy::cast_sign_loss)]
        let len = par.number as usize * usize::from(par.contracts[0].level != 0);

        #[allow(clippy::cast_sign_loss)]
        let contracts = par.contracts[..len]
            .iter()
            .flat_map(|contract| {
                let strain = [
                    Strain::Notrump,
                    Strain::Spades,
                    Strain::Hearts,
                    Strain::Diamonds,
                    Strain::Clubs,
                ][contract.denom as usize];

                // SAFETY: the assertions inside ensure successful conversion
                #[allow(clippy::cast_possible_truncation)]
                let (penalty, overtricks) = if contract.underTricks > 0 {
                    assert!(contract.underTricks <= 13);
                    (Penalty::Doubled, -contract.underTricks as i8)
                } else {
                    assert!(contract.overTricks >= 0 && contract.overTricks <= 13);
                    (Penalty::Undoubled, contract.overTricks as i8)
                };

                let seat = match contract.seats & 3 {
                    0 => Seat::North,
                    1 => Seat::East,
                    2 => Seat::South,
                    3 => Seat::West,
                    _ => unreachable!("The bitmask ensures this is always in 0..=3"),
                };
                let is_pair = contract.seats >= 4;
                #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
                let contract = Contract::new(contract.level as u8, strain, penalty);

                core::iter::once(ParContract {
                    contract,
                    declarer: seat,
                    overtricks,
                })
                .chain(is_pair.then_some(ParContract {
                    contract,
                    declarer: seat.partner(),
                    overtricks,
                }))
            })
            .collect();

        Self {
            score: par.score,
            contracts,
        }
    }
}
