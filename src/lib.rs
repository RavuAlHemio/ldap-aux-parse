use std::{cmp::Ordering, fmt};

mod common_parsing;
pub mod dn;
pub mod filter;
#[cfg(feature = "serde")] mod ser_de;
#[cfg(feature = "stringprep")] mod stringprep;


/// Tracing macro that does nothing when the `tracing` feature is disabled.
#[macro_export]
macro_rules! no_trace {
    ($($toks:tt)*) => {};
}


/// A slice of a textual or binary string.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum StringSlice<'a> {
    /// A slice that is valid UTF-8.
    Textual { index: usize, string: &'a str },

    /// A byte that is not part of a valid UTF-8 sequence.
    Byte { index: usize, byte: u8 },
}

/// Converts a hexadecimal digit to its corresponding nibble (4-byte) value.
///
/// Recognized digits are `'0'` through `'9'` (zero through nine), `'A'` through `'F'` (ten through
/// fifteen), and `'a'` through `'f'` (again, ten through fifteen).
pub(crate) fn hex_to_nibble(hex: char) -> u8 {
    match hex {
        '0'..='9' => ((hex as u32) - ('0' as u32)) as u8,
        'A'..='F' => ((hex as u32) + 10 - ('A' as u32)) as u8,
        'a'..='f' => ((hex as u32) + 10 - ('a' as u32)) as u8,
        _ => unreachable!(),
    }
}

/// Converts a binary string into a sequence of textual and binary slices.
pub(crate) fn byte_string_to_slices(s: &[u8]) -> Vec<StringSlice> {
    let mut slices = Vec::new();
    let mut index = 0;
    while s[index..].len() > 0 {
        match std::str::from_utf8(&s[index..]) {
            Ok(string) => {
                slices.push(StringSlice::Textual { index, string });
                index += string.len();
            },
            Err(e) => {
                let last_str_index = e.valid_up_to();
                if last_str_index > 0 {
                    let string = std::str::from_utf8(&s[index..index+last_str_index]).unwrap();
                    slices.push(StringSlice::Textual { index, string });
                }
                index += last_str_index;
                slices.push(StringSlice::Byte { index, byte: s[index] });
                index += 1;
            },
        }
    }
    slices
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
        let (rest, key) = crate::common_parsing::parse_attribute_key(s).ok()?;
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
        let (rest, key) = crate::common_parsing::parse_attribute_key_short(s).ok()?;
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
        let (rest, key) = crate::common_parsing::parse_attribute_key_oid(s).ok()?;
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

    /// Attempts to parse this attribute value from a string, applying DN escaping rules.
    ///
    /// This is the function to use if the attribute value is passed as part of a distinguished name
    /// (DN) or relative distinguished name (RDN); the syntax is specified in RFC4514.
    pub fn try_from_dn_str(s: &str) -> Option<Self> {
        let (rest, key) = crate::dn::parsing::parse_rdn_value(s).ok()?;
        if rest.len() > 0 {
            return None;
        }
        Some(key)
    }

    /// Attempts to parse this attribute value from a string, applying filter escaping rules.
    ///
    /// This is the function to use if the attribute value is passed as part of an LDAP filter
    /// string; the syntax is specified in RFC4515.
    pub fn try_from_filter_str(s: &str) -> Option<Self> {
        let (rest, key) = crate::filter::parsing::parse_assertion_value(s).ok()?;
        if rest.len() > 0 {
            return None;
        }
        Some(key)
    }

    /// Wraps this `AttributeValue` in `Some(_)` if it contains any bytes and returns `None` if it
    /// is empty.
    pub fn into_option(self) -> Option<Self> {
        if self.bytes.len() > 0 {
            Some(self)
        } else {
            None
        }
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

#[cfg(test)]
mod tests {
    use super::AttributeValue;

    #[test]
    fn test_parse_attribute_value() {
        let value = AttributeValue::try_from_dn_str("").unwrap();
        assert_eq!(value.bytes, b"");

        let value = AttributeValue::try_from_dn_str("abc").unwrap();
        assert_eq!(value.bytes, b"abc");

        let value = AttributeValue::try_from_dn_str("#616263").unwrap();
        assert_eq!(value.bytes, b"abc");

        let value = AttributeValue::try_from_dn_str("Dewey\\, Cheatham & Howe").unwrap();
        assert_eq!(value.bytes, b"Dewey, Cheatham & Howe");

        let value = AttributeValue::try_from_dn_str("\\ leading space").unwrap();
        assert_eq!(value.bytes, b" leading space");

        let value = AttributeValue::try_from_dn_str("\\   leading spaces").unwrap();
        assert_eq!(value.bytes, b"   leading spaces");

        let value = AttributeValue::try_from_dn_str("trailing space\\ ").unwrap();
        assert_eq!(value.bytes, b"trailing space ");

        let value = AttributeValue::try_from_dn_str("trailing spaces  \\ ").unwrap();
        assert_eq!(value.bytes, b"trailing spaces   ");

        let value = AttributeValue::try_from_dn_str("\\#").unwrap();
        assert_eq!(value.bytes, b"#");

        let value = AttributeValue::try_from_dn_str("#23").unwrap();
        assert_eq!(value.bytes, b"#");

        assert_eq!(AttributeValue::try_from_dn_str("Dewey, Cheatham & Howe"), None);
        assert_eq!(AttributeValue::try_from_dn_str(" leading space"), None);
        assert_eq!(AttributeValue::try_from_dn_str("trailing space "), None);
        assert_eq!(AttributeValue::try_from_dn_str("#61\\36\\3263"), None);
    }
}
