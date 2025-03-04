//! Structures related to Distinguished Names (DNs).


pub(crate) mod parsing;


use std::collections::BTreeSet;
use std::fmt;

use crate::{AttributeType, AttributeValue};


/// The unique identifier of a specific object in the directory.
///
/// The concept of a distinguished name, along its ASN.1 representation, is defined in the ITU-T
/// standard X.501. The string representation is defined in RFC4514.
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DistinguishedName(Vec<RelativeDistinguishedName>);
impl DistinguishedName {
    /// Obtains the slice of the relative distinguished names that are the basis of this
    /// distinguished name.
    pub fn as_rdns(&self) -> &[RelativeDistinguishedName] {
        &self.0
    }

    /// Converts this distinguished name into a vector of relative distinguished names.
    pub fn into_rdns(self) -> Vec<RelativeDistinguishedName> {
        self.0
    }

    /// Attempts to parse this distinguished name from a string.
    pub fn try_from_str(s: &str) -> Option<Self> {
        let (rest, dn) = crate::dn::parsing::parse_dn(s).ok()?;
        if rest.len() > 0 {
            return None;
        }
        Some(dn)
    }
}
impl From<Vec<RelativeDistinguishedName>> for DistinguishedName {
    fn from(value: Vec<RelativeDistinguishedName>) -> Self {
        Self(value)
    }
}
impl fmt::Display for DistinguishedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first_rdn = true;
        for rdn in &self.0 {
            if first_rdn {
                first_rdn = false;
            } else {
                write!(f, ",")?;
            }
            write!(f, "{}", rdn)?;
        }
        Ok(())
    }
}

/// A single component of a distinguished name.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct RelativeDistinguishedName {
    key_value_pairs: BTreeSet<(AttributeType, AttributeValue)>,
}
impl RelativeDistinguishedName {
    /// Creates a new RDN from a single key-value pair.
    pub fn new_single(first_type: AttributeType, first_value: AttributeValue) -> Self {
        let mut key_value_pairs = BTreeSet::new();
        key_value_pairs.insert((first_type, first_value));
        Self {
            key_value_pairs,
        }
    }

    /// Returns this RDN as a set of key-value pairs.
    pub fn as_set(&self) -> &BTreeSet<(AttributeType, AttributeValue)> {
        &self.key_value_pairs
    }

    /// Attempts to parse this relative distinguished name from a string.
    pub fn try_from_str(s: &str) -> Option<Self> {
        let (rest, rdn) = crate::dn::parsing::parse_rdn(s).ok()?;
        if rest.len() > 0 {
            return None;
        }
        Some(rdn)
    }
}
impl fmt::Display for RelativeDistinguishedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first_kvp = true;
        for (key, value) in &self.key_value_pairs {
            if first_kvp {
                first_kvp = false;
            } else {
                write!(f, "+")?;
            }
            write!(f, "{}={}", key, value)?;
        }
        Ok(())
    }
}

/// An error which can occur during parsing of a distinguished name.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ParseError {
    /// The attribute type is empty.
    EmptyType,

    /// The attribute type contains an invalid byte.
    InvalidByteInType { byte: u8, pos: usize },

    /// An arc of an OID is empty.
    ///
    /// Arcs are the components of an OID. For example, in the OID `1.0.8802.1`, the arcs are `1`,
    /// `0`, `8802` and `1`.
    EmptyOidArc,

    /// A hex string consists of an odd number of hex digits.
    OddLengthHexString,

    /// A hex string contains a byte that is not a valid hex digit.
    InvalidByteInHexString { byte: u8, pos: usize },

    /// A string contains a byte that may not appear within a string.
    InvalidByte { byte: u8, pos: usize },

    /// A string contains a character that may not appear within a string.
    InvalidChar { c: char },

    /// A string contains a byte that cannot be backslash-escaped.
    ///
    /// The byte may be allowed to be escaped in a different context or not at all.
    InvalidEscapedByte { byte: u8 },

    /// A string contains an invalid UTF-8 sequence.
    ///
    /// `byte_pos` points at the invalid byte.
    InvalidUtf8Sequence { pos: usize },

    /// A string ends with an incomplete escape sequence.
    IncompleteFinalEscape,

    /// A string ends with an incomplete UTF-8 sequence.
    IncompleteFinalUtf8Sequence,

    /// A relative distinguished name (RDN) is actually multiple RDNs.
    MultipleRdns { count: usize },

    /// A byte has occurred an unexpected amount of times.
    WrongNumberByteOccurrences { byte: u8, expected: usize, obtained: usize },
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyType
                => write!(f, "empty attribute type"),
            Self::InvalidByteInType { byte, pos }
                => write!(f, "invalid character 0x{:02X} at byte position {} in attribute type", byte, pos),
            Self::EmptyOidArc
                => write!(f, "OID contains an empty arc"),
            Self::OddLengthHexString
                => write!(f, "hex string has an odd number of hex digits"),
            Self::InvalidByteInHexString { byte, pos: byte_pos }
                => write!(f, "hex string contains an invalid character (byte 0x{:02X}) at byte position {}", byte, byte_pos),
            Self::InvalidByte { byte, pos }
                => write!(f, "invalid byte 0x{:02X} at position {}", byte, pos),
            Self::InvalidChar { c }
                => write!(f, "invalid character {:?}", c),
            Self::InvalidEscapedByte { byte }
                => write!(f, "byte 0x{:02X} cannot be backslash-escaped", byte),
            Self::IncompleteFinalEscape
                => write!(f, "string ends with an incomplete escape sequence"),
            Self::InvalidUtf8Sequence { pos }
                => write!(f, "invalid UTF-8 sequence at position {}", pos),
            Self::IncompleteFinalUtf8Sequence
                => write!(f, "string ends with incomplete UTF-8 sequence"),
            Self::MultipleRdns { count }
                => write!(f, "RDN is actually {} RDNs", count),
            Self:: WrongNumberByteOccurrences { byte, expected, obtained }
                => write!(f, "expected {} occurrences of byte 0x{:02X}; obtained {}", expected, byte, obtained),
        }
    }
}
impl std::error::Error for ParseError {
}

#[cfg(test)]
mod tests {
    use super::DistinguishedName;

    #[test]
    fn test_parse_dn() {
        let value = DistinguishedName::try_from_str("dc=example,dc=com").unwrap();
        assert_eq!(value.as_rdns().len(), 2);
        assert_eq!(value.as_rdns()[0].as_set().len(), 1);
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_short().unwrap(), "dc");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().1.as_bytes(), b"example");
        assert_eq!(value.as_rdns()[1].as_set().len(), 1);
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().0.as_short().unwrap(), "dc");
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().1.as_bytes(), b"com");

        let value = DistinguishedName::try_from_str("o=Dewey\\, Cheatham & Howe,l=London,c=GB").unwrap();
        assert_eq!(value.as_rdns().len(), 3);
        assert_eq!(value.as_rdns()[0].as_set().len(), 1);
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_short().unwrap(), "o");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().1.as_bytes(), b"Dewey, Cheatham & Howe");
        assert_eq!(value.as_rdns()[1].as_set().len(), 1);
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().0.as_short().unwrap(), "l");
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().1.as_bytes(), b"London");
        assert_eq!(value.as_rdns()[2].as_set().len(), 1);
        assert_eq!(value.as_rdns()[2].as_set().first().unwrap().0.as_short().unwrap(), "c");
        assert_eq!(value.as_rdns()[2].as_set().first().unwrap().1.as_bytes(), b"GB");

        let value = DistinguishedName::try_from_str("2.5.4.10=Dewey\\, Cheatham & Howe,l=London,c=GB").unwrap();
        assert_eq!(value.as_rdns().len(), 3);
        assert_eq!(value.as_rdns()[0].as_set().len(), 1);
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_oid().unwrap().len(), 4);
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_oid().unwrap()[0], "2");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_oid().unwrap()[1], "5");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_oid().unwrap()[2], "4");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_oid().unwrap()[3], "10");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().1.as_bytes(), b"Dewey, Cheatham & Howe");
        assert_eq!(value.as_rdns()[1].as_set().len(), 1);
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().0.as_short().unwrap(), "l");
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().1.as_bytes(), b"London");
        assert_eq!(value.as_rdns()[2].as_set().len(), 1);
        assert_eq!(value.as_rdns()[2].as_set().first().unwrap().0.as_short().unwrap(), "c");
        assert_eq!(value.as_rdns()[2].as_set().first().unwrap().1.as_bytes(), b"GB");

        let value = DistinguishedName::try_from_str("l=Biel+l=Bienne,c=CH").unwrap();
        assert_eq!(value.as_rdns().len(), 2);
        assert_eq!(value.as_rdns()[0].as_set().len(), 2);
        assert_eq!(value.as_rdns()[0].as_set().iter().nth(0).unwrap().0.as_short().unwrap(), "l");
        assert_eq!(value.as_rdns()[0].as_set().iter().nth(0).unwrap().1.as_bytes(), b"Biel");
        assert_eq!(value.as_rdns()[0].as_set().iter().nth(1).unwrap().0.as_short().unwrap(), "l");
        assert_eq!(value.as_rdns()[0].as_set().iter().nth(1).unwrap().1.as_bytes(), b"Bienne");
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().0.as_short().unwrap(), "c");
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().1.as_bytes(), b"CH");
    }
}
