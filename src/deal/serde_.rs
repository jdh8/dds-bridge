use super::{Card, FullDeal, Hand, Holding, Seat, Subset};
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

impl Serialize for Card {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        serialize(self, s)
    }
}
impl<'de> Deserialize<'de> for Card {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        deserialize(d)
    }
}

impl Serialize for Holding {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        serialize(self, s)
    }
}
impl<'de> Deserialize<'de> for Holding {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        deserialize(d)
    }
}

impl Serialize for Hand {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        serialize(self, s)
    }
}
impl<'de> Deserialize<'de> for Hand {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        deserialize(d)
    }
}

impl Serialize for Subset {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.collect_str(&self.display(Seat::North))
    }
}
impl<'de> Deserialize<'de> for Subset {
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
