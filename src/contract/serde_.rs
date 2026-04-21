use super::{Bid, Contract};
use core::fmt::Display;
use core::str::FromStr;
use serde::{Deserialize, Deserializer, Serialize, Serializer, de};

fn serialize<T: Display, S: Serializer>(value: &T, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.collect_str(value)
}

fn deserialize<'de, T, D>(deserializer: D) -> Result<T, D::Error>
where
    T: FromStr,
    T::Err: Display,
    D: Deserializer<'de>,
{
    let s = <&str>::deserialize(deserializer)?;
    s.parse().map_err(de::Error::custom)
}

impl Serialize for Bid {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        serialize(self, s)
    }
}
impl<'de> Deserialize<'de> for Bid {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        deserialize(d)
    }
}

impl Serialize for Contract {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        serialize(self, s)
    }
}
impl<'de> Deserialize<'de> for Contract {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        deserialize(d)
    }
}
