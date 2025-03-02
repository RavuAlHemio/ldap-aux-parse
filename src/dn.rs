//! Functionality related to parsing Distinguished Names (DNs).

use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt;
use std::str::FromStr;

use crate::{ByteStringPart, find_next_index, FromByteStringParts, split_byte_string_parts_at};


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
}
impl<'a> FromByteStringParts<'a> for DistinguishedName {
    type Error = ParseError;

    fn from_byte_string_parts(parts: &[ByteStringPart<'a>]) -> Result<Self, Self::Error> {
        let rdn_parts = split_byte_string_parts_at(parts, b',', 0);
        let mut rdns = Vec::with_capacity(rdn_parts.len());

        for rdn_part in rdn_parts {
            let kvps = split_byte_string_parts_at(&rdn_part, b'+', 0);
            let mut key_value_pairs = BTreeSet::new();
            for kvp in kvps {
                let key_and_val = split_byte_string_parts_at(&kvp, b'=', 0);
                if key_and_val.len() != 2 {
                    return Err(ParseError::WrongNumberByteOccurrences {
                        byte: b'=',
                        expected: 1,
                        obtained: key_and_val.len(),
                    });
                }

                let key = &key_and_val[0];
                let value = &key_and_val[1];

                let key = AttributeType::from_byte_string_parts(key)?;
                let value = AttributeValue::from_byte_string_parts(value)?;
                key_value_pairs.insert((key, value));
            }
            rdns.push(RelativeDistinguishedName { key_value_pairs });
        }
        Ok(Self(rdns))
    }
}
impl FromStr for DistinguishedName {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = process_dn_escapes(s)?;
        Self::from_byte_string_parts(&parts)
    }
}

/// A single component of a distinguished name.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct RelativeDistinguishedName {
    key_value_pairs: BTreeSet<(AttributeType, AttributeValue)>,
}
impl RelativeDistinguishedName {
    pub fn new(first_type: AttributeType, first_value: AttributeValue) -> Self {
        let mut key_value_pairs = BTreeSet::new();
        key_value_pairs.insert((first_type, first_value));
        Self {
            key_value_pairs,
        }
    }

    pub fn as_set(&self) -> &BTreeSet<(AttributeType, AttributeValue)> {
        &self.key_value_pairs
    }
}
impl<'a> FromByteStringParts<'a> for RelativeDistinguishedName {
    type Error = ParseError;

    fn from_byte_string_parts(parts: &[ByteStringPart<'a>]) -> Result<Self, Self::Error> {
        let dn = DistinguishedName::from_byte_string_parts(parts)?;
        let mut rdns = dn.into_rdns();
        if rdns.len() != 1 {
            return Err(ParseError::MultipleRdns { count: rdns.len() });
        }
        Ok(rdns.swap_remove(0))
    }
}
impl FromStr for RelativeDistinguishedName {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = process_dn_escapes(s)?;
        Self::from_byte_string_parts(&parts)
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
}
impl<'a> FromByteStringParts<'a> for AttributeType {
    type Error = ParseError;
    fn from_byte_string_parts(parts: &[ByteStringPart<'a>]) -> Result<Self, Self::Error> {
        // try the OID attribute format first
        if let Ok(o) = OidAttributeType::from_byte_string_parts(parts) {
            Ok(Self::Oid(o))
        } else {
            ShortAttributeType::from_byte_string_parts(parts)
                .map(|sat| Self::Short(sat))
        }
    }
}
impl FromStr for AttributeType {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = process_dn_escapes(s)?;
        Self::from_byte_string_parts(&parts)
    }
}

/// The name of an attribute in a short textual format.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ShortAttributeType {
    name: String,
}
impl ShortAttributeType {
    /// Creates a new short attribute type (descriptor), ensuring its validity.
    ///
    /// Valid descriptors are defined by RFC4512, section 1.4, production `descr`.
    pub fn new(name: &str) -> Result<Self, ParseError> {
        let parts = process_dn_escapes(name)?;
        Self::from_byte_string_parts(&parts)
    }

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
}
impl FromStr for ShortAttributeType {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s)
    }
}
impl<'a> FromByteStringParts<'a> for ShortAttributeType {
    type Error = ParseError;

    fn from_byte_string_parts(parts: &[ByteStringPart<'a>]) -> Result<Self, Self::Error> {
        // no escapes allowed here
        ensure_no_escapes(parts)?;
        let unescaped = parts[0].as_unescaped().unwrap();

        // descr = keystring
        // keystring = leadkeychar *keychar
        // leadkeychar = ALPHA
        // keychar = ALPHA / DIGIT / HYPHEN
        // ALPHA = "A"-"Z" / "a"-"z"
        // DIGIT = "0"-"9"
        // HYPHEN = "-"
        let initial_byte = match unescaped.get(0) {
            Some(b) => *b,
            None => return Err(ParseError::EmptyType),
        };
        let initial_byte_valid =
            (initial_byte >= b'A' && initial_byte <= b'Z')
            || (initial_byte >= b'a' && initial_byte <= b'z');
        if !initial_byte_valid {
            return Err(ParseError::InvalidByteInType { byte: initial_byte, pos: 0 });
        }
        for (i, b) in unescaped.iter().copied().enumerate().skip(1) {
            let this_char_valid =
                (b >= b'A' && b <= b'Z')
                || (b >= b'a' && b <= b'z')
                || (b >= b'0' && b <= b'9')
                || b == b'-';
            if !this_char_valid {
                return Err(ParseError::InvalidByteInType { byte: b, pos: i });
            }
        }
        let name = std::str::from_utf8(unescaped).unwrap();
        Ok(Self {
            name: name.to_owned(),
        })
    }
}

/// The name of an attribute as an OID.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct OidAttributeType {
    // parts can be any length => store them as strings
    arcs: Vec<String>,
}
impl OidAttributeType {
    /// Creates a new OID attribute type, ensuring its validity.
    ///
    /// Valid descriptors are defined by RFC4512, section 1.4, production `numericoid`.
    pub fn new(name: &str) -> Result<Self, ParseError> {
        let parts = process_dn_escapes(name)?;
        Self::from_byte_string_parts(&parts)
    }

    /// Returns the arcs of this attribute type OID as decimal numeric strings.
    pub fn as_strings(&self) -> &[String] {
        &self.arcs
    }
}
impl<'a> FromByteStringParts<'a> for OidAttributeType {
    type Error = ParseError;

    fn from_byte_string_parts(parts: &[ByteStringPart<'a>]) -> Result<Self, Self::Error> {
        // no escapes allowed here
        ensure_no_escapes(parts)?;

        // numericoid = number 1*( DOT number )
        // number = DIGIT / ( LDIGIT 1*DIGIT )
        // LDIGIT = "1"-"9"
        // DIGIT = "0"-"9"
        // DOT = "."
        // => no leading zeroes on nonzero numbers

        let dot_count = parts[0].as_unescaped().unwrap().iter().filter(|b| **b == b'.').count();
        let mut arcs = Vec::with_capacity(dot_count + 1);

        let dot_pieces = split_byte_string_parts_at(&parts, b'.', 0);
        for dot_piece in dot_pieces {
            assert_eq!(dot_piece.len(), 1);
            assert!(dot_piece[0].as_unescaped().is_some());

            let digits = dot_piece[0].as_unescaped().unwrap();
            if digits == b"0" {
                // fast-track this exception to the "no leading zeroes" rule
                arcs.push("0".to_owned());
                continue;
            }
            let initial_byte = match digits.iter().nth(0) {
                Some(b) => *b,
                None => return Err(ParseError::EmptyOidArc),
            };
            let initial_byte_valid = initial_byte >= b'1' && initial_byte <= b'9';
            if !initial_byte_valid {
                return Err(ParseError::InvalidByteInType {
                    byte: initial_byte,
                    pos: dot_piece[0].index(),
                });
            }

            for (i, b) in digits.iter().copied().enumerate().skip(1) {
                let this_char_valid = b >= b'0' && b <= b'9';
                if !this_char_valid {
                    return Err(ParseError::InvalidByteInType {
                        byte: b,
                        pos: dot_piece[0].index() + i,
                    });
                }
            }

            let digits_str = std::str::from_utf8(digits).unwrap();
            arcs.push(digits_str.to_owned());
        }

        Ok(Self {
            arcs,
        })
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
impl FromStr for OidAttributeType {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s)
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
}
impl<'a> FromByteStringParts<'a> for AttributeValue {
    type Error = ParseError;

    fn from_byte_string_parts(parts: &[ByteStringPart<'a>]) -> Result<Self, Self::Error> {
        if parts.len() == 0 {
            return Ok(Self { bytes: Vec::with_capacity(0) });
        }
        if parts.len() == 1 && parts[0].as_unescaped().map(|p| p.starts_with(b"#")).unwrap_or(false) {
            // hex string (may not contain escapes)
            let unescaped = parts[0].as_unescaped().unwrap();
            let total_bytes = unescaped.len();
            if total_bytes % 2 != 1 {
                // number of hex digits must be even => number of characters including # must be odd
                return Err(ParseError::OddLengthHexString);
            }

            let mut bytes = Vec::with_capacity((total_bytes - 1) / 2);
            for (hex_byte_index, hex_byte) in unescaped[1..].chunks(2).enumerate() {
                assert_eq!(hex_byte.len(), 2);
                let mut actual_byte = 0;
                for (nibble_index, &nibble) in hex_byte.iter().enumerate() {
                    actual_byte *= 0x10;
                    actual_byte += match nibble {
                        b'0'..=b'9' => nibble - b'0',
                        b'A'..=b'F' => nibble - b'A' + 10,
                        b'a'..=b'f' => nibble - b'a' + 10,
                        _ => return Err(ParseError::InvalidByteInHexString {
                            byte: nibble,
                            pos: 1 + 2*hex_byte_index + nibble_index,
                        }),
                    };
                }
                bytes.push(actual_byte);
            }

            Ok(Self {
                bytes,
            })
        } else {
            // textual
            let expected_length = parts
                .iter()
                .map(|part| part.len())
                .sum();
            let mut bytes = Vec::with_capacity(expected_length);
            let mut first_char = true;
            let mut parts_iter = parts.iter().peekable();
            while let Some(part) = parts_iter.next() {
                match part {
                    ByteStringPart::EscapedByte { byte, .. } => {
                        // just add it verbatim (and don't consider it the first character)
                        bytes.push(*byte);
                        first_char = false;
                    },
                    ByteStringPart::UnescapedSlice { slice, index } => {
                        let mut slice_bytes = slice.iter().copied().enumerate().peekable();

                        while let Some((i, b)) = slice_bytes.next() {
                            // check general validity
                            // forbidden bytes are: 0x00, 0x22 ("), 0x2B (+), 0x2C (,), 0x3B (;),
                            // 0x3C (<), 0x3E (>), 0x5C (\)
                            // and invalid UTF-8 sequences

                            if b == 0x00 || b == b'"' || b == b'+' || b == b',' || b == b';'
                                    || b == b'<' || b == b'>' || b == b'\\' {
                                return Err(ParseError::InvalidByte {
                                    byte: b,
                                    pos: index + i,
                                });
                            }

                            if first_char {
                                first_char = false;

                                // check validity of first byte
                                // also forbidden are 0x20 ( ) and 0x23 (#)
                                if b == b' ' || b == b'#' {
                                    return Err(ParseError::InvalidByte {
                                        byte: b,
                                        pos: index + i,
                                    });
                                }
                            } else if slice_bytes.peek().is_none() && parts_iter.peek().is_none() {
                                // check validity of last byte
                                // also forbidden is 0x20 ( )
                                if b == b' ' {
                                    return Err(ParseError::InvalidByte {
                                        byte: b,
                                        pos: index + i,
                                    });
                                }
                            }

                            // check UTF-8 validity
                            if b & 0b1000_0000 == 0b0000_0000 {
                                // low ASCII, gotta love it
                                // 0b0xxx_xxxx
                                bytes.push(b);
                            } else if b & 0b1100_0000 == 0b1000_0000 {
                                // UTF-8 multibyte sequence cannot start with a continuation byte
                                return Err(ParseError::InvalidUtf8Sequence {
                                    pos: index + i,
                                });
                            } else if b & 0b1110_0000 == 0b1100_0000 {
                                // 0b110x_xxxx 0b10xx_xxxx
                                let (i2, b2) = match slice_bytes.next() {
                                    Some(ib) => ib,
                                    None => return Err(ParseError::IncompleteFinalUtf8Sequence),
                                };
                                if b2 & 0b1100_0000 != 0b1000_0000 {
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i2,
                                    });
                                }
                                let char_index =
                                    ((b & 0b0001_1111) as u32) << 6
                                    | ((b2 & 0b0011_1111) as u32) << 0
                                ;
                                if char_index <= 0b111_1111 {
                                    // violation of the "use shortest encoding" rule
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i,
                                    });
                                }
                                if char::from_u32(char_index).is_none() {
                                    // encoding a UTF-16 surrogate
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i,
                                    });
                                }
                                bytes.push(b);
                                bytes.push(b2);
                            } else if b & 0b1111_0000 == 0b1110_0000 {
                                // 0b1110_xxxx 0b10xx_xxxx 0b10xx_xxxx
                                let (i2, b2) = match slice_bytes.next() {
                                    Some(ib) => ib,
                                    None => return Err(ParseError::IncompleteFinalUtf8Sequence),
                                };
                                if b2 & 0b1100_0000 != 0b1000_0000 {
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i2,
                                    });
                                }
                                let (i3, b3) = match slice_bytes.next() {
                                    Some(ib) => ib,
                                    None => return Err(ParseError::IncompleteFinalUtf8Sequence),
                                };
                                if b3 & 0b1100_0000 != 0b1000_0000 {
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i3,
                                    });
                                }
                                let char_index =
                                    ((b & 0b0000_1111) as u32) << 12
                                    | ((b2 & 0b0011_1111) as u32) << 6
                                    | ((b3 & 0b0011_1111) as u32) << 0
                                ;
                                if char_index <= 0b1_1111_11_1111 {
                                    // violation of the "use shortest encoding" rule
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i,
                                    });
                                }
                                if char::from_u32(char_index).is_none() {
                                    // encoding a UTF-16 surrogate
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i,
                                    });
                                }
                                bytes.push(b);
                                bytes.push(b2);
                                bytes.push(b3);
                            } else if b & 0b1111_1000 == 0b1111_0000 {
                                // 0b1111_0xxx 0b10xx_xxxx 0b10xx_xxxx 0b10xx_xxxx
                                let (i2, b2) = match slice_bytes.next() {
                                    Some(ib) => ib,
                                    None => return Err(ParseError::IncompleteFinalUtf8Sequence),
                                };
                                if b2 & 0b1100_0000 != 0b1000_0000 {
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i2,
                                    });
                                }
                                let (i3, b3) = match slice_bytes.next() {
                                    Some(ib) => ib,
                                    None => return Err(ParseError::IncompleteFinalUtf8Sequence),
                                };
                                if b3 & 0b1100_0000 != 0b1000_0000 {
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i3,
                                    });
                                }
                                let (i4, b4) = match slice_bytes.next() {
                                    Some(ib) => ib,
                                    None => return Err(ParseError::IncompleteFinalUtf8Sequence),
                                };
                                if b4 & 0b1100_0000 != 0b1000_0000 {
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i4,
                                    });
                                }
                                let char_index =
                                    ((b & 0b0000_0111) as u32) << 18
                                    | ((b2 & 0b0011_1111) as u32) << 12
                                    | ((b3 & 0b0011_1111) as u32) << 6
                                    | ((b4 & 0b0011_1111) as u32) << 0
                                ;
                                if char_index <= 0b1111_11_1111_11_1111 {
                                    // violation of the "use shortest encoding" rule
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i,
                                    });
                                }
                                if char::from_u32(char_index).is_none() {
                                    // encoding a UTF-16 surrogate
                                    return Err(ParseError::InvalidUtf8Sequence {
                                        pos: index + i,
                                    });
                                }
                                bytes.push(b);
                                bytes.push(b2);
                                bytes.push(b3);
                                bytes.push(b4);
                            } else {
                                // UTF-8 would allow longer encodings but Unicode does not
                                return Err(ParseError::InvalidUtf8Sequence {
                                    pos: index + i,
                                });
                            }
                        }
                    },
                }
            }

            Ok(Self {
                bytes,
            })
        }
    }
}
impl FromStr for AttributeValue {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let escaped = process_dn_escapes(s)?;
        AttributeValue::from_byte_string_parts(&escaped)
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


fn process_dn_escapes(dn: &str) -> Result<Vec<ByteStringPart>, ParseError> {
    let dn_bytes = dn.as_bytes();
    let mut next_index = Some(0);
    let mut parts = Vec::new();
    while let Some(mut index) = next_index {
        if let Some(backslash_index) = find_next_index(dn_bytes, b'\\', index) {
            let unescaped_preceding = &dn_bytes[index..backslash_index];
            if unescaped_preceding.len() > 0 {
                parts.push(ByteStringPart::UnescapedSlice { index, slice: unescaped_preceding });
            }

            // process the escape
            index = backslash_index + 1;
            let b1 = match dn_bytes.get(index) {
                Some(b) => *b,
                None => return Err(ParseError::IncompleteFinalEscape),
            };
            match b1 {
                b' '|b'#'|b'='|b'"'|b'+'|b','|b';'|b'<'|b'>'|b'\\' => {
                    // self-escape
                    parts.push(ByteStringPart::EscapedByte { byte: b1, index: backslash_index });
                    next_index = Some(index + 1);
                },
                b'0'..=b'9'|b'A'..=b'F'|b'a'..=b'f' => {
                    // hex escape (hopefully)
                    index += 1;

                    let top_nibble = match b1 {
                        b'0'..=b'9' => b1 - b'0',
                        b'A'..=b'F' => b1 + 10 - b'A',
                        b'a'..=b'f' => b1 + 10 - b'a',
                        _ => unreachable!(),
                    };
                    assert_eq!(top_nibble & 0xF0, 0);

                    let b2 = match dn_bytes.get(index) {
                        Some(b) => *b,
                        None => return Err(ParseError::IncompleteFinalEscape),
                    };
                    match b2 {
                        b'0'..=b'9'|b'A'..=b'F'|b'a'..=b'f' => {
                            index += 1;

                            // yup, hex escape
                            let bottom_nibble = match b2 {
                                b'0'..=b'9' => b2 - b'0',
                                b'A'..=b'F' => b2 + 10 - b'A',
                                b'a'..=b'f' => b2 + 10 - b'a',
                                _ => unreachable!(),
                            };
                            assert_eq!(bottom_nibble & 0xF0, 0);

                            let byte = top_nibble * 0x10 + bottom_nibble;
                            parts.push(ByteStringPart::EscapedByte { byte, index: backslash_index });
                            next_index = Some(index);
                        },
                        _ => return Err(ParseError::InvalidEscapedByte { byte: b2 }),
                    }
                },
                _ => return Err(ParseError::InvalidEscapedByte { byte: b1 }),
            }
        } else {
            // no more backslash
            let unescaped_final = dn[index..].as_bytes();
            if unescaped_final.len() > 0 {
                parts.push(ByteStringPart::UnescapedSlice { index, slice: unescaped_final });
            }

            next_index = None;
        };
    }
    Ok(parts)
}

fn ensure_no_escapes(parts: &[ByteStringPart]) -> Result<(), ParseError> {
    if parts.len() == 0 {
        return Err(ParseError::EmptyType);
    }
    if let Some(esc) = parts[0].as_escaped() {
        return Err(ParseError::InvalidEscapedByte { byte: esc });
    }
    if parts.len() > 1 {
        assert!(parts[0].as_unescaped().is_some());
        assert!(parts[1].as_escaped().is_some());
        return Err(ParseError::InvalidEscapedByte { byte: parts[1].as_escaped().unwrap() });
    }
    assert_eq!(parts.len(), 1);
    assert!(parts[0].as_unescaped().is_some());
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::{AttributeValue, DistinguishedName, ParseError, process_dn_escapes};
    use std::str::FromStr;
    use crate::ByteStringPart;

    #[test]
    fn test_process_dn_escapes() {
        let pieces = process_dn_escapes("").unwrap();
        assert_eq!(pieces.len(), 0);

        let pieces = process_dn_escapes("dc=example,dc=com").unwrap();
        assert_eq!(pieces.len(), 1);
        assert_eq!(pieces[0], ByteStringPart::UnescapedSlice { slice: b"dc=example,dc=com", index: 0 });

        let pieces = process_dn_escapes("dc=example\\,dc=com").unwrap();
        assert_eq!(pieces.len(), 3);
        assert_eq!(pieces[0], ByteStringPart::UnescapedSlice { slice: b"dc=example", index: 0 });
        assert_eq!(pieces[1], ByteStringPart::EscapedByte { byte: b',', index: 10 });
        assert_eq!(pieces[2], ByteStringPart::UnescapedSlice { slice: b"dc=com", index: 12 });

        let pieces = process_dn_escapes("dc=\\=\\=equals-signs\\=\\=,dc=com").unwrap();
        assert_eq!(pieces.len(), 7);
        assert_eq!(pieces[0], ByteStringPart::UnescapedSlice { slice: b"dc=", index: 0 });
        assert_eq!(pieces[1], ByteStringPart::EscapedByte { byte: b'=', index: 3 });
        assert_eq!(pieces[2], ByteStringPart::EscapedByte { byte: b'=', index: 5 });
        assert_eq!(pieces[3], ByteStringPart::UnescapedSlice { slice: b"equals-signs", index: 7 });
        assert_eq!(pieces[4], ByteStringPart::EscapedByte { byte: b'=', index: 19 });
        assert_eq!(pieces[5], ByteStringPart::EscapedByte { byte: b'=', index: 21 });
        assert_eq!(pieces[6], ByteStringPart::UnescapedSlice { slice: b",dc=com", index: 23 });

        let pieces = process_dn_escapes("o=Dewey\\, Cheatham & Howe,c=GB").unwrap();
        assert_eq!(pieces.len(), 3);
        assert_eq!(pieces[0], ByteStringPart::UnescapedSlice { slice: b"o=Dewey", index: 0 });
        assert_eq!(pieces[1], ByteStringPart::EscapedByte { byte: b',', index: 7 });
        assert_eq!(pieces[2], ByteStringPart::UnescapedSlice { slice: b" Cheatham & Howe,c=GB", index: 9 });
    }

    #[test]
    fn test_parse_attribute_value() {
        let value = AttributeValue::from_str("").unwrap();
        assert_eq!(value.bytes, b"");

        let value = AttributeValue::from_str("abc").unwrap();
        assert_eq!(value.bytes, b"abc");

        let value = AttributeValue::from_str("#616263").unwrap();
        assert_eq!(value.bytes, b"abc");

        let value = AttributeValue::from_str("Dewey\\, Cheatham & Howe").unwrap();
        assert_eq!(value.bytes, b"Dewey, Cheatham & Howe");

        let value = AttributeValue::from_str("\\ leading space").unwrap();
        assert_eq!(value.bytes, b" leading space");

        let value = AttributeValue::from_str("\\   leading spaces").unwrap();
        assert_eq!(value.bytes, b"   leading spaces");

        let value = AttributeValue::from_str("trailing space\\ ").unwrap();
        assert_eq!(value.bytes, b"trailing space ");

        let value = AttributeValue::from_str("trailing spaces  \\ ").unwrap();
        assert_eq!(value.bytes, b"trailing spaces   ");

        let value = AttributeValue::from_str("\\#").unwrap();
        assert_eq!(value.bytes, b"#");

        let value = AttributeValue::from_str("#23").unwrap();
        assert_eq!(value.bytes, b"#");

        let err = AttributeValue::from_str("Dewey, Cheatham & Howe").unwrap_err();
        assert_eq!(err, ParseError::InvalidByte { byte: b',', pos: 5 });

        let err = AttributeValue::from_str(" leading space").unwrap_err();
        assert_eq!(err, ParseError::InvalidByte { byte: b' ', pos: 0 });

        let err = AttributeValue::from_str("trailing space ").unwrap_err();
        assert_eq!(err, ParseError::InvalidByte { byte: b' ', pos: 14 });

        let err = AttributeValue::from_str("#61\\36\\3263").unwrap_err();
        assert_eq!(err, ParseError::InvalidByte { byte: b'#', pos: 0 });
    }

    #[test]
    fn test_parse_dn() {
        let value = DistinguishedName::from_str("dc=example,dc=com").unwrap();
        assert_eq!(value.as_rdns().len(), 2);
        assert_eq!(value.as_rdns()[0].as_set().len(), 1);
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_short().unwrap(), "dc");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().1.as_bytes(), b"example");
        assert_eq!(value.as_rdns()[1].as_set().len(), 1);
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().0.as_short().unwrap(), "dc");
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().1.as_bytes(), b"com");

        let value = DistinguishedName::from_str("o=Dewey\\, Cheatham & Howe,l=London,c=GB").unwrap();
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

        let value = DistinguishedName::from_str("2.5.4.10=Dewey\\, Cheatham & Howe,l=London,c=GB").unwrap();
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

        let value = DistinguishedName::from_str("l=Biel+l=Bienne,c=CH").unwrap();
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
