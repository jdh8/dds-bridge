use super::{FullDeal, Seat, PartialDeal};
use core::fmt::Display;
use core::str::FromStr;
use serde::{Deserialize, Deserializer, Serialize, Serializer, de};

fn deserialize<'de, T, D>(deserializer: D) -> Result<T, D::Error>
where
    T: FromStr,
    T::Err: Display,
    D: Deserializer<'de>,
{
    let s = <&str>::deserialize(deserializer)?;
    s.parse().map_err(de::Error::custom)
}

impl Serialize for PartialDeal {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.collect_str(&self.display(Seat::North))
    }
}
impl<'de> Deserialize<'de> for PartialDeal {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        deserialize(d)
    }
}

impl Serialize for FullDeal {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.collect_str(&self.display(Seat::North))
    }
}
impl<'de> Deserialize<'de> for FullDeal {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        deserialize(d)
    }
}
