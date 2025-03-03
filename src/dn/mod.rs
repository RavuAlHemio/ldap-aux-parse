//! Functionality related to parsing Distinguished Names (DNs).
//!
//! The string representation of distinguished names is defined in RFC4514.


mod parsing;


use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt;

use crate::{byte_string_to_slices, StringSlice};


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

/// The name of an attribute.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AttributeType {
    Short(ShortAttributeType),
    Oid(OidAttributeType)
}
impl AttributeType {
    /// `Some(s)` if this is an attribute with a short name (where `s` is the short name); `None`
    /// otherwise.
    pub fn as_short(&self) -> Option<&str> {
        match self {
            Self::Short(s) => Some(s.name.as_str()),
            _ => None,
        }
    }

    /// `Some(arcs)` if this is an attribute with an OID name (where `arcs` are the arcs of the OID
    /// as strings); `None` otherwise.
    pub fn as_oid(&self) -> Option<&[String]> {
        match self {
            Self::Oid(o) => Some(o.arcs.as_slice()),
            _ => None,
        }
    }

    /// Attempts to parse this attribute type from a string.
    pub fn try_from_str(s: &str) -> Option<Self> {
        let (rest, key) = crate::dn::parsing::parse_rdn_key(s).ok()?;
        if rest.len() > 0 {
            return None;
        }
        Some(key)
    }
}
impl fmt::Display for AttributeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Short(sat) => sat.fmt(f),
            Self::Oid(oat) => oat.fmt(f),
        }
    }
}

/// The name of an attribute in a short textual format.
///
/// Valid short attribute types (descriptors) are defined by RFC4512, section 1.4, production
/// `descr`.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ShortAttributeType {
    pub(crate) name: String,
}
impl ShortAttributeType {
    /// Returns the string representation of this short attribute type.
    pub fn as_str(&self) -> &str {
        &self.name
    }

    /// Compares this short attribute type to another in a case-insensitive fashion.
    pub fn compare_case_insensitive(&self, other: &ShortAttributeType) -> Ordering {
        // good news: short names are ASCII
        self.as_str().to_ascii_lowercase()
            .cmp(&other.as_str().to_ascii_lowercase())
    }

    /// Attempts to parse this short attribute type from a string.
    pub fn try_from_str(s: &str) -> Option<Self> {
        let (rest, key) = crate::dn::parsing::parse_rdn_key_short(s).ok()?;
        if rest.len() > 0 {
            return None;
        }
        Some(key)
    }
}
impl fmt::Display for ShortAttributeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // nothing to escape; format does not allow any characters that would need escaping
        self.name.fmt(f)
    }
}

/// The name of an attribute as an OID.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct OidAttributeType {
    // parts can be any length => store them as strings
    pub(crate) arcs: Vec<String>,
}
impl OidAttributeType {
    /// Returns the arcs of this attribute type OID as decimal numeric strings.
    pub fn as_strings(&self) -> &[String] {
        &self.arcs
    }

    /// Attempts to parse this OID attribute type from a string.
    pub fn try_from_str(s: &str) -> Option<Self> {
        let (rest, key) = crate::dn::parsing::parse_rdn_key_oid(s).ok()?;
        if rest.len() > 0 {
            return None;
        }
        Some(key)
    }
}
impl Ord for OidAttributeType {
    fn cmp(&self, other: &Self) -> Ordering {
        for (self_part, other_part) in self.arcs.iter().zip(other.arcs.iter()) {
            // good news: no leading zeroes, no negative numbers, no decimal point
            // => we can compare number lengths, then break ties by comparing ASCIIbetically
            let compared = self_part.len().cmp(&other_part.len())
                .then_with(|| self_part.as_bytes().cmp(other_part.as_bytes()));
            if compared.is_ne() {
                return compared;
            }
        }

        // initial parts are all the same; let the lengths break the tie
        self.arcs.len().cmp(&other.arcs.len())
    }
}
impl PartialOrd for OidAttributeType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl fmt::Display for OidAttributeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // nothing to escape; format does not allow any characters that would need escaping
        let mut first_arc = true;
        for arc in &self.arcs {
            if first_arc {
                first_arc = false;
            } else {
                write!(f, ".")?;
            }
            write!(f, "{}", arc)?;
        }
        Ok(())
    }
}

/// The value of an attribute.
///
/// This may be represented as a textual (UTF-8) string or a hex string; semantically, they are
/// equivalent.
///
/// Textual strings must be valid UTF-8; byte sequences violating UTF-8 must be escaped.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct AttributeValue {
    bytes: Vec<u8>,
}
impl AttributeValue {
    /// The bytes corresponding to this attribute value.
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Attempts to parse this attribute value from a string.
    pub fn try_from_str(s: &str) -> Option<Self> {
        let (rest, key) = crate::dn::parsing::parse_rdn_value(s).ok()?;
        if rest.len() > 0 {
            return None;
        }
        Some(key)
    }
}
impl fmt::Display for AttributeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::fmt::Write;

        // try the text-with-escapes representation first
        let slices = byte_string_to_slices(&self.bytes);
        let mut text_with_escapes = String::new();
        for (slice_index, slice) in slices.iter().enumerate() {
            match slice {
                StringSlice::Byte { byte, .. } => {
                    write!(&mut text_with_escapes, "\\{:02X}", byte)?;
                },
                StringSlice::Textual { index, string } => {
                    assert!(string.len() > 0);

                    let mut actual_string = *string;
                    if *index == 0 {
                        // first part of string
                        if actual_string.starts_with("#") || actual_string.starts_with(" ") {
                            // DNs may not start with these characters; escape them
                            write!(text_with_escapes, "\\{}", &actual_string[0..1])?;
                            actual_string = &actual_string[1..];
                        }
                    }

                    let mut append_me = "";
                    if slice_index == slices.len() - 1 {
                        // last part of string
                        if string.ends_with(" ") {
                            // DNs may not end with these characters; prepare to escape them
                            append_me = "\\ ";
                            actual_string = &actual_string[..actual_string.len()-1];
                        }
                    }

                    for c in actual_string.chars() {
                        match c {
                            '\u{00}' => write!(text_with_escapes, "\\00")?,
                            '"'|'+'|','|';'|'<'|'>'|'\\' => write!(text_with_escapes, "\\{}", c)?,
                            other => write!(text_with_escapes, "{}", other)?,
                        }
                    }

                    text_with_escapes.push_str(append_me);
                },
            }
        }
        let hex_length = 1 + 2*self.bytes.len();
        if text_with_escapes.len() > hex_length {
            // prefer hex representation
            write!(f, "#")?;
            for &b in &self.bytes {
                write!(f, "{:02X}", b)?;
            }
        } else {
            write!(f, "{}", text_with_escapes)?;
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
