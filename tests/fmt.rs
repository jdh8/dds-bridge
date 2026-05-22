use core::fmt::{Debug, Display};
use core::str::FromStr;

use dds_bridge::Vulnerability;

/// Assert that `value` survives a `Display` → `FromStr` round-trip.
fn assert_roundtrip<T>(value: T)
where
    T: Copy + Debug + Display + FromStr + PartialEq,
    <T as FromStr>::Err: Debug + PartialEq,
{
    assert_eq!(value.to_string().parse::<T>(), Ok(value));
}

#[test]
fn vulnerability_roundtrip() {
    for vul in [
        Vulnerability::NONE,
        Vulnerability::NS,
        Vulnerability::EW,
        Vulnerability::ALL,
    ] {
        assert_roundtrip(vul);
    }
}
