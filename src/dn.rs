//! Functionality related to parsing Distinguished Names (DNs).

use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt;
use std::str::FromStr;

use crate::{find_next_index, ByteStringPart, FromByteStringParts};


/// The unique identifier of a specific object in the directory.
///
/// The concept of a distinguished name, along its ASN.1 representation, is defined in the ITU-T
/// standard X.501. The string representation is defined in RFC4514.
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DistinguishedName(Vec<RelativeDistinguishedName>);

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

/// The name of an attribute.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AttributeType {
    Short(ShortAttributeType),
    Oid(OidAttributeType)
}
impl FromStr for AttributeType {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // try the OID attribute format first
        if let Ok(o) = OidAttributeType::new(s) {
            Ok(Self::Oid(o))
        } else {
            ShortAttributeType::new(s)
                .map(|sat| Self::Short(sat))
        }
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
        // descr = keystring
        // keystring = leadkeychar *keychar
        // leadkeychar = ALPHA
        // keychar = ALPHA / DIGIT / HYPHEN
        // ALPHA = "A"-"Z" / "a"-"z"
        // DIGIT = "0"-"9"
        // HYPHEN = "-" 
        let initial_char = match name.chars().nth(0) {
            Some(c) => c,
            None => return Err(ParseError::EmptyType),
        };
        let initial_char_valid =
            (initial_char >= 'A' && initial_char <= 'Z')
            || (initial_char >= 'a' && initial_char <= 'z');
        if !initial_char_valid {
            return Err(ParseError::InvalidCharInType { character: initial_char, byte_pos: 0 });
        }
        for (i, c) in name.char_indices().skip(1) {
            let this_char_valid =
                (c >= 'A' && c <= 'Z')
                || (c >= 'a' && c <= 'z')
                || (c >= '0' && c <= '9')
                || c == '-';
            if !this_char_valid {
                return Err(ParseError::InvalidCharInType { character: c, byte_pos: i });
            }
        }
        Ok(Self {
            name: name.to_owned(),
        })
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

/// The name of an attribute as an OID.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct OidAttributeType {
    // parts can be any length => store them as strings
    parts: Vec<String>,
}
impl OidAttributeType {
    /// Creates a new OID attribute type, ensuring its validity.
    ///
    /// Valid descriptors are defined by RFC4512, section 1.4, production `numericoid`.
    pub fn new(name: &str) -> Result<Self, ParseError> {
        // numericoid = number 1*( DOT number )
        // number = DIGIT / ( LDIGIT 1*DIGIT )
        // LDIGIT = "1"-"9"
        // DIGIT = "0"-"9"
        // DOT = "."
        // => no leading zeroes on nonzero numbers

        let dot_count = name.bytes().filter(|b| *b == b'.').count();
        let mut parts = Vec::with_capacity(dot_count + 1);

        let mut index_opt = Some(0);
        while let Some(index) = index_opt {
            // this basically emulates name.split('.'), except we keep track of the index
            let part = match find_next_index(name, '.', index) {
                Some(dot_index) => {
                    index_opt = Some(dot_index + '.'.len_utf8());
                    &name[index..dot_index]
                },
                None => {
                    // this is it
                    index_opt = None;
                    &name[index..]
                },
            };

            if part == "0" {
                // fast-track this exception to the "no leading zeroes" rule
                parts.push(part.to_owned());
                continue;
            }
            let initial_char = match part.chars().nth(0) {
                Some(c) => c,
                None => return Err(ParseError::EmptyOidArc),
            };
            let initial_char_valid = initial_char >= '1' && initial_char <= '9';
            if !initial_char_valid {
                return Err(ParseError::InvalidCharInType { character: initial_char, byte_pos: index });
            }

            for (i, c) in name.char_indices().skip(1) {
                let this_char_valid = c >= '0' && c <= '9';
                if !this_char_valid {
                    return Err(ParseError::InvalidCharInType { character: c, byte_pos: index + i });
                }
            }

            parts.push(part.to_owned());
        }

        Ok(Self {
            parts,
        })
    }

    /// Returns the parts of this attribute type OID as decimal numeric strings.
    pub fn as_strings(&self) -> &[String] {
        &self.parts
    }
}
impl Ord for OidAttributeType {
    fn cmp(&self, other: &Self) -> Ordering {
        for (self_part, other_part) in self.parts.iter().zip(other.parts.iter()) {
            // good news: no leading zeroes, no negative numbers, no decimal point
            // => we can compare number lengths, then break ties by comparing ASCIIbetically
            let compared = self_part.len().cmp(&other_part.len())
                .then_with(|| self_part.as_bytes().cmp(other_part.as_bytes()));
            if compared.is_ne() {
                return compared;
            }
        }

        // initial parts are all the same; let the lengths break the tie
        self.parts.len().cmp(&other.parts.len())
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
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct AttributeValue {
    bytes: Vec<u8>,
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
                        _ => return Err(ParseError::InvalidCharInHexString {
                            byte: nibble,
                            byte_pos: 1 + 2*hex_byte_index + nibble_index,
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
            for part in parts {
                match part {
                    ByteStringPart::EscapedByte { byte, .. } => {
                        // just add it verbatim
                        bytes.push(*byte);
                    },
                    ByteStringPart::UnescapedSlice { slice, index } => {
                        let mut bytes = slice.iter().peekable();
                        let mut is_first = true;

                        while let Some(&b) = bytes.next() {
                            // check general validity
                            // forbidden bytes are: 0x00, 0x22 ("), 0x2B (+), 0x2C (,), 0x3B (;),
                            // 0x3C (<), 0x3E (>), 0x5C (\)
                            // and invalid UTF-8 sequences

                            if b == 0x00 || b == b'"' || b == b'+' || b == b',' || b == b';'
                                    || b == b'<' || b == b'>' || b == b'\\' {
                                todo!();
                            }

                            if is_first {
                                is_first = false;

                                // check validity of first byte
                                todo!();
                            } else if bytes.peek().is_none() {
                                // check validity of last byte
                            }
                        }
                    },
                }
            }
            while let Some((i, c)) = characters.next() {
                // check for general validity
                let is_valid =
                    (c >= '\u{01}' && c <= '\u{21}')
                    || (c >= '\u{23}' && c <= '\u{2A}')
                    || (c >= '\u{2D}' && c <= '\u{3A}')
                    || c == '\u{3D}'
                    || (c >= '\u{3F}' && c <= '\u{5B}')
                    || (c >= '\u{5D}' && c <= '\u{7F}')
                    || c >= '\u{80}';
                if !is_valid {
                    return Err(ParseError::InvalidChar { character: c, byte_pos: i });
                }

                if first_char {
                    first_char = false;

                    // also check for first-character validity
                    let is_valid_first =
                        c != '\u{20}'
                        || c != '\u{23}';
                    if !is_valid_first {
                        return Err(ParseError::InvalidInitialChar { character: c });
                    }
                } else if characters.peek().is_none() {
                    // check for last-character validity
                    let is_valid_last =
                        c != '\u{20}';
                    if !is_valid_last {
                        return Err(ParseError::InvalidFinalChar { character: c });
                    }
                }

                let mut bf = [0u8; 4];
                bytes.extend_from_slice(c.encode_utf8(&mut bf).as_bytes());
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
        if s.len() == 0 {
            return Ok(Self { bytes: Vec::with_capacity(0) });
        }

        if s.starts_with("#") {
            // hex string
            let total_bytes = s.as_bytes().len();
            if total_bytes % 2 != 1 {
                // number of hex digits must be even => number of characters including # must be odd
                return Err(ParseError::OddLengthHexString);
            }

            let mut bytes = Vec::with_capacity((total_bytes - 1) / 2);
            for (hex_byte_index, hex_byte) in s.as_bytes()[1..].chunks(2).enumerate() {
                assert_eq!(hex_byte.len(), 2);
                let mut actual_byte = 0;
                for (nibble_index, &nibble) in hex_byte.iter().enumerate() {
                    actual_byte *= 0x10;
                    actual_byte += match nibble {
                        b'0'..=b'9' => nibble - b'0',
                        b'A'..=b'F' => nibble - b'A' + 10,
                        b'a'..=b'f' => nibble - b'a' + 10,
                        _ => return Err(ParseError::InvalidCharInHexString {
                            byte: nibble,
                            byte_pos: 1 + 2*hex_byte_index + nibble_index,
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

            // process escapes first
            let s_with_escapes = process_dn_escapes(s)?;
            if s_with_escapes.len() == 0 {
                return Ok(Self { bytes: Vec::with_capacity(0) });
            }

            let mut characters = s.char_indices().peekable();
            let mut bytes = Vec::with_capacity(s.len());
            let mut first_char = true;
            while let Some((i, c)) = characters.next() {
                if c == '\\' {
                    // escaping!

                    // if we start out with an escape, we don't need to give special treatment
                    // to the first character
                    first_char = false;

                    // what's the next character?
                    let c2 = characters.next().map(|(_ii, cc)| cc);
                    match c2 {
                        None => {
                            // string ends with backslash
                            return Err(ParseError::InvalidFinalChar { character: c });
                        },
                        Some(' ')|Some('#')|Some('=')|Some('"')|Some('+')|Some(',')|Some(';')
                                |Some('<')|Some('>')|Some('\\') => {
                            // direct escape
                            bytes.push((c2.unwrap() as u32) as u8);
                        },
                        Some(other) => {
                            // hex digit?
                            let top_nibble = match other {
                                '0'..='9' => ((other as u32) - ('0' as u32)) as u8,
                                'A'..='F' => ((other as u32) + 10 - ('A' as u32)) as u8,
                                'a'..='f' => ((other as u32) + 10 - ('a' as u32)) as u8,
                                _ => return Err(ParseError::InvalidEscapedChar { character: other }),
                            };

                            // another hex digit?
                            let c3 = characters.next().map(|(_ii, cc)| cc);
                            match c3 {
                                None => {
                                    // e.g. "\B" at end
                                    return Err(ParseError::IncompleteFinalEscape);
                                },
                                Some(another) => {
                                    let bottom_nibble = match another {
                                        '0'..='9' => ((another as u32) - ('0' as u32)) as u8,
                                        'A'..='F' => ((another as u32) + 10 - ('A' as u32)) as u8,
                                        'a'..='f' => ((another as u32) + 10 - ('a' as u32)) as u8,
                                        _ => return Err(ParseError::InvalidEscapedChar { character: another }),
                                    };

                                    let byte = (top_nibble * 0x10) + bottom_nibble;
                                    bytes.push(byte);
                                },
                            }
                        },
                    };

                    continue;
                }

                // check for general validity
                let is_valid =
                    (c >= '\u{01}' && c <= '\u{21}')
                    || (c >= '\u{23}' && c <= '\u{2A}')
                    || (c >= '\u{2D}' && c <= '\u{3A}')
                    || c == '\u{3D}'
                    || (c >= '\u{3F}' && c <= '\u{5B}')
                    || (c >= '\u{5D}' && c <= '\u{7F}')
                    || c >= '\u{80}';
                if !is_valid {
                    return Err(ParseError::InvalidChar { character: c, byte_pos: i });
                }

                if first_char {
                    first_char = false;

                    // also check for first-character validity
                    let is_valid_first =
                        c != '\u{20}'
                        || c != '\u{23}';
                    if !is_valid_first {
                        return Err(ParseError::InvalidInitialChar { character: c });
                    }
                } else if characters.peek().is_none() {
                    // check for last-character validity
                    let is_valid_last =
                        c != '\u{20}';
                    if !is_valid_last {
                        return Err(ParseError::InvalidFinalChar { character: c });
                    }
                }

                let mut bf = [0u8; 4];
                bytes.extend_from_slice(c.encode_utf8(&mut bf).as_bytes());
            }

            Ok(Self {
                bytes,
            })
        }
    }
}

/// An error which can occur during parsing of a distinguished name.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ParseError {
    /// The attribute type is empty.
    EmptyType,

    /// The attribute type contains an invalid character.
    ///
    /// `byte_pos` is the index of the first byte of the invalid character within the string. Byte
    /// instead of character indexing was chosen because string slicing in Rust works on byte
    /// indexes.
    InvalidCharInType { character: char, byte_pos: usize },

    /// An arc of an OID is empty.
    ///
    /// Arcs are the components of an OID. For example, in the OID `1.0.8802.1`, the arcs are `1`,
    /// `0`, `8802` and `1`.
    EmptyOidArc,

    /// A hex string consists of an odd number of hex digits.
    OddLengthHexString,

    /// A hex string contains a character that is not a valid hex digit.
    InvalidCharInHexString { byte: u8, byte_pos: usize },

    /// A string starts with a character that may not appear at the beginning of a string.
    InvalidInitialChar { character: char },

    /// A string contains with a character that may not appear within a string.
    InvalidChar { character: char, byte_pos: usize },

    /// A string ends with a character that may not appear at the end of a string.
    InvalidFinalChar { character: char },

    /// A string contains a character that cannot be backslash-escaped.
    InvalidEscapedChar { character: char },

    /// A string ends with an incomplete escape sequence.
    IncompleteFinalEscape,
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyType
                => write!(f, "empty attribute type"),
            Self::InvalidCharInType { character, byte_pos }
                => write!(f, "invalid character {:?} at byte position {} in attribute type", character, byte_pos),
            Self::EmptyOidArc
                => write!(f, "OID contains an empty arc"),
            Self::OddLengthHexString
                => write!(f, "hex string has an odd number of hex digits"),
            Self::InvalidCharInHexString { byte, byte_pos }
                => write!(f, "hex string contains an invalid character (byte 0x{:02X}) at byte position {}", byte, byte_pos),
            Self::InvalidInitialChar { character }
                => write!(f, "invalid character {:?} at start of string", character),
            Self::InvalidChar { character, byte_pos}
                => write!(f, "invalid character {:?} at byte position {}", character, byte_pos),
            Self::InvalidFinalChar { character }
                => write!(f, "invalid character {:?} at end of string", character),
            Self::InvalidEscapedChar { character }
                => write!(f, "character {:?} cannot be backslash-escaped", character),
            Self::IncompleteFinalEscape
                => write!(f, "string ends with an incomplete escape sequence"),
        }
    }
}
impl std::error::Error for ParseError {
}


fn process_dn_escapes(dn: &str) -> Result<Vec<ByteStringPart>, ParseError> {
    const BACKSLASH_LEN: usize = '\\'.len_utf8();

    let mut next_index = Some(0);
    let mut parts = Vec::new();
    while let Some(mut index) = next_index {
        if let Some(backslash_index) = find_next_index(dn, '\\', index) {
            let unescaped_preceding = dn[index..backslash_index].as_bytes();
            if unescaped_preceding.len() > 0 {
                parts.push(ByteStringPart::UnescapedSlice(unescaped_preceding));
            }

            // process the escape
            index += '\\'.len_utf8();
            let c1 = match dn[index..].chars().nth(0) {
                Some(c) => c,
                None => return Err(ParseError::IncompleteFinalEscape),
            };
            match c1 {
                ' '|'#'|'='|'"'|'+'|','|';'|'<'|'>'|'\\' => {
                    // self-escape
                    parts.push(ByteStringPart::EscapedByte((c1 as u32) as u8));
                    next_index = Some(index + c1.len_utf8());
                },
                '0'..='9'|'A'..='F'|'a'..='f' => {
                    // hex escape (hopefully)
                    index += c1.len_utf8();

                    let top_nibble = match c1 {
                        '0'..='9' => ((c1 as u32) - ('0' as u32)) as u8,
                        'A'..='F' => ((c1 as u32) + 10 - ('A' as u32)) as u8,
                        'a'..='f' => ((c1 as u32) + 10 - ('a' as u32)) as u8,
                        _ => unreachable!(),
                    };
                    assert_eq!(top_nibble & 0xF0, 0);

                    let c2 = match dn[index..].chars().nth(0) {
                        Some(c) => c,
                        None => return Err(ParseError::IncompleteFinalEscape),
                    };
                    match c2 {
                        '0'..='9'|'A'..='F'|'a'..='f' => {
                            index += c2.len_utf8();

                            // yup, hex escape
                            let bottom_nibble = match c2 {
                                '0'..='9' => ((c2 as u32) - ('0' as u32)) as u8,
                                'A'..='F' => ((c2 as u32) + 10 - ('A' as u32)) as u8,
                                'a'..='f' => ((c2 as u32) + 10 - ('a' as u32)) as u8,
                                _ => unreachable!(),
                            };
                            assert_eq!(bottom_nibble & 0xF0, 0);

                            let byte = top_nibble * 0x10 + bottom_nibble;
                            parts.push(ByteStringPart::EscapedByte(byte));
                            next_index = Some(index);
                        },
                        _ => return Err(ParseError::InvalidEscapedChar { character: c2 }),
                    }
                },
                _ => return Err(ParseError::InvalidEscapedChar { character: c1 }),
            }
        } else {
            // no more backslash
            let unescaped_final = dn[index..].as_bytes();
            if unescaped_final.len() > 0 {
                parts.push(ByteStringPart::UnescapedSlice(unescaped_final));
            }

            next_index = None;
        };
    }
    Ok(parts)
}
