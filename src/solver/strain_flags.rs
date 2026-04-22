//! Strain selection flags for batch solving

bitflags::bitflags! {
    /// Flags for the solver to solve for a strain
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    pub struct StrainFlags : u8 {
        /// Solve for clubs ([`Strain::Clubs`](crate::Strain::Clubs))
        const CLUBS = 0x01;
        /// Solve for diamonds ([`Strain::Diamonds`](crate::Strain::Diamonds))
        const DIAMONDS = 0x02;
        /// Solve for hearts ([`Strain::Hearts`](crate::Strain::Hearts))
        const HEARTS = 0x04;
        /// Solve for spades ([`Strain::Spades`](crate::Strain::Spades))
        const SPADES = 0x08;
        /// Solve for notrump ([`Strain::Notrump`](crate::Strain::Notrump))
        const NOTRUMP = 0x10;
    }
}

/// A guaranteed non-empty [`StrainFlags`]
///
/// Analogous to [`NonZero`](core::num::NonZero) — constructable only if the
/// flags are non-empty, ensuring callers cannot accidentally pass an empty set
/// to functions that require at least one strain.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NonEmptyStrainFlags(StrainFlags);

impl NonEmptyStrainFlags {
    /// All strains
    pub const ALL: Self = Self(StrainFlags::all());

    /// Wrap `flags` if non-empty, otherwise return `None`
    #[must_use]
    pub const fn new(flags: StrainFlags) -> Option<Self> {
        if flags.is_empty() {
            None
        } else {
            Some(Self(flags))
        }
    }

    /// Extract the inner [`StrainFlags`]
    #[must_use]
    pub const fn get(self) -> StrainFlags {
        self.0
    }
}

impl From<NonEmptyStrainFlags> for StrainFlags {
    #[inline]
    fn from(flags: NonEmptyStrainFlags) -> Self {
        flags.0
    }
}
