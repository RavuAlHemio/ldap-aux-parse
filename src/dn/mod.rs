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

    /// Attempts to create a new RDN from a set of key-value pairs.
    ///
    /// Returns `None` if `key_value_pairs` is empty.
    pub fn try_from_key_value_pairs(key_value_pairs: BTreeSet<(AttributeType, AttributeValue)>) -> Option<Self> {
        if key_value_pairs.len() == 0 {
            None
        } else {
            Some(Self {
                key_value_pairs,
            })
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
