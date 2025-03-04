use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::de::Error as _;


use crate::{AttributeType, AttributeValue, OidAttributeType, ShortAttributeType};
use crate::dn::{DistinguishedName, RelativeDistinguishedName};
use crate::filter::{AttributeDescription, Filter};


impl Serialize for AttributeType {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Self::Oid(o) => o.serialize(serializer),
            Self::Short(s) => s.serialize(serializer),
        }
    }
}
impl<'de> Deserialize<'de> for AttributeType {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let stringified = String::deserialize(deserializer)?;
        Self::try_from_str(&stringified)
            .ok_or(D::Error::custom("invalid syntax"))
    }
}

impl Serialize for AttributeValue {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let stringified = self.to_string();
        stringified.serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for AttributeValue {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let stringified = String::deserialize(deserializer)?;
        Self::try_from_filter_str(&stringified)
            .ok_or(D::Error::custom("invalid syntax (LDAP filter syntax expected)"))
    }
}

impl Serialize for OidAttributeType {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let stringified = self.to_string();
        stringified.serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for OidAttributeType {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let stringified = String::deserialize(deserializer)?;
        Self::try_from_str(&stringified)
            .ok_or(D::Error::custom("invalid syntax"))
    }
}

impl Serialize for ShortAttributeType {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let stringified = self.to_string();
        stringified.serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for ShortAttributeType {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let stringified = String::deserialize(deserializer)?;
        Self::try_from_str(&stringified)
            .ok_or(D::Error::custom("invalid syntax"))
    }
}

impl Serialize for DistinguishedName {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let stringified = self.to_string();
        stringified.serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for DistinguishedName {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let stringified = String::deserialize(deserializer)?;
        Self::try_from_str(&stringified)
            .ok_or(D::Error::custom("invalid syntax"))
    }
}

impl Serialize for RelativeDistinguishedName {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let stringified = self.to_string();
        stringified.serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for RelativeDistinguishedName {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let stringified = String::deserialize(deserializer)?;
        Self::try_from_str(&stringified)
            .ok_or(D::Error::custom("invalid syntax"))
    }
}

impl Serialize for AttributeDescription {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let stringified = self.to_string();
        stringified.serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for AttributeDescription {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let stringified = String::deserialize(deserializer)?;
        Self::try_from_str(&stringified)
            .ok_or(D::Error::custom("invalid syntax"))
    }
}

impl Serialize for Filter {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let stringified = self.to_string();
        stringified.serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for Filter {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let stringified = String::deserialize(deserializer)?;
        Self::try_from_str(&stringified)
            .ok_or(D::Error::custom("invalid syntax"))
    }
}
