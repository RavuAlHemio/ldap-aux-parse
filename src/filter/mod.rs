//! Structures related to LDAP filters.
//!
//! The string representation of LDAP filters is defined in RFC4515.


pub(crate) mod parsing;


use std::collections::BTreeSet;
use std::fmt;

use crate::{AttributeType, AttributeValue};
use crate::filter::parsing::{parse_attribute_description, parse_filter};


/// An attribute value being asserted.
///
/// On the protocol level, assertion and attribute values are equivalent.
pub type AssertionValue = AttributeValue;


/// An attribute description, which captures the type of an attribute as well as a set of zero or
/// more options.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct AttributeDescription {
    pub attribute_type: AttributeType,
    pub options: BTreeSet<LdapOption>,
}
impl AttributeDescription {
    /// Attempts to parse this attribute value from a string, applying LDAP filter escaping rules.
    pub fn try_from_str(s: &str) -> Option<Self> {
        let (rest, attr) = parse_attribute_description(s).ok()?;
        if rest.len() > 0 {
            return None;
        }
        Some(attr)
    }
}
impl fmt::Display for AttributeDescription {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.attribute_type)?;
        for option in &self.options {
            write!(f, ";{}", option.as_str())?;
        }
        Ok(())
    }
}


/// An LDAP option.
///
/// Options can be attached to attribute types to form attribute descriptions.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct LdapOption {
    pub(crate) name: String,
}
impl LdapOption {
    pub fn as_str(&self) -> &str { &self.name }
    pub fn into_string(self) -> String { self.name }
}

/// An LDAP filter.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Filter {
    /// A set of multiple criteria that must all be met for an entry to match the filter.
    And(BTreeSet<Filter>),

    /// A set of multiple criteria of which at least one must be met for an entry to match the
    /// filter.
    Or(BTreeSet<Filter>),

    /// A criterion that matches those entries which do not match the inner filter.
    Not(Box<Filter>),

    /// A criterion which matches when the attribute identified by the key is equal to the value.
    Equality { key: AttributeDescription, value: AssertionValue },

    /// A criterion which matches when the attribute identified by the key matches the given
    /// pattern.
    ///
    /// To visualize the filter, between each string in the pattern, imagine a placeholder that can
    /// represent any sequence of bytes, including an empty one.
    Substring {
        key: AttributeDescription,
        start: Option<AssertionValue>,
        middle: Vec<AssertionValue>,
        end: Option<AssertionValue>,
    },

    /// A criterion which matches when the value of the attribute identified by the key is greater
    /// than or equal to the provided value.
    GreaterOrEqual { key: AttributeDescription, value: AssertionValue },

    /// A criterion which matches when the value of the attribute identified by the key is smaller
    /// than or equal to the provided value.
    LessOrEqual { key: AttributeDescription, value: AssertionValue },

    /// A criterion which matches when the attribute identified by the key has at least one value.
    Present { key: AttributeDescription },

    /// A criterion which matches when the value of the attribute identified by the key is
    /// approximately equal to the provided value.
    Approximate { key: AttributeDescription, value: AssertionValue },

    /// A specialized criterion.
    Extensible {
        matching_rule: Option<AttributeType>,
        key: Option<AttributeDescription>,
        value: AssertionValue,
        dn_attributes: bool,
    },
}
impl Filter {
    /// Attempts to parse this attribute value from a string, applying LDAP filter escaping rules.
    pub fn try_from_str(s: &str) -> Option<Self> {
        let (rest, attr) = parse_filter(s).ok()?;
        if rest.len() > 0 {
            return None;
        }
        Some(attr)
    }

    pub fn is_and_filter(&self) -> bool { matches!(self, Filter::And(_)) }
    pub fn is_or_filter(&self) -> bool { matches!(self, Filter::Or(_)) }
    pub fn is_not_filter(&self) -> bool { matches!(self, Filter::Not(_)) }
    pub fn is_equality_filter(&self) -> bool { matches!(self, Filter::Equality { .. }) }
    pub fn is_substring_filter(&self) -> bool { matches!(self, Filter::Substring { .. }) }
    pub fn is_greater_or_equal_filter(&self) -> bool { matches!(self, Filter::GreaterOrEqual { .. }) }
    pub fn is_less_or_equal_filter(&self) -> bool { matches!(self, Filter::LessOrEqual { .. }) }
    pub fn is_present_filter(&self) -> bool { matches!(self, Filter::Present { .. }) }
    pub fn is_approximate_filter(&self) -> bool { matches!(self, Filter::Approximate { .. }) }
    pub fn is_extensible_filter(&self) -> bool { matches!(self, Filter::Extensible { .. }) }

    /// Returns, for `And` and `Or` filters, the set of subfilters.
    ///
    /// Returns `None` for all other filter types.
    pub fn subfilters(&self) -> Option<&BTreeSet<Filter>> {
        match self {
            Self::And(subfilters) => Some(subfilters),
            Self::Or(subfilters) => Some(subfilters),
            _ => None,
        }
    }

    /// Returns, for `Not` filters, the subfilter being negated.
    ///
    /// Returns `None` for all other filter types.
    pub fn negated_filter(&self) -> Option<&Filter> {
        match self {
            Self::Not(negated) => Some(&*negated),
            _ => None,
        }
    }

    /// Returns, for `Equality`, `Substring`, `GreaterOrEqual`, `LessOrEqual`, `Present` and
    /// `Approximate` filters, as well as `Extensible` filters where the key is set, the key.
    ///
    /// Returns `None` otherwise.
    pub fn key(&self) -> Option<&AttributeDescription> {
        match self {
            Self::Equality { key, .. } => Some(key),
            Self::Substring { key, .. } => Some(key),
            Self::GreaterOrEqual { key, .. } => Some(key),
            Self::LessOrEqual { key, .. } => Some(key),
            Self::Present { key, .. } => Some(key),
            Self::Approximate { key, .. } => Some(key),
            Self::Extensible { key, .. } => key.as_ref(),
            Self::And(_)|Self::Or(_)|Self::Not(_) => None,
        }
    }

    /// Returns, for `Equality`, `GreaterOrEqual`, `LessOrEqual`, `Approximate` and `Extensible`
    /// filters, the value against which the filter compares.
    ///
    /// Returns `None` for all other filter types.
    pub fn value(&self) -> Option<&AssertionValue> {
        match self {
            Self::Equality { value, .. } => Some(value),
            Self::GreaterOrEqual { value, .. } => Some(value),
            Self::LessOrEqual { value, .. } => Some(value),
            Self::Approximate { value, .. } => Some(value),
            Self::Extensible { value, .. } => Some(value),
            Self::And(_)|Self::Or(_)|Self::Not(_)|Self::Present { .. }|Self::Substring { .. } => None,
        }
    }

    /// Returns, for `Substring` matches, the fixed string preceding the first placeholder, if any.
    ///
    /// Returns `None` for all other filter types.
    pub fn substring_start(&self) -> Option<&AssertionValue> {
        match self {
            Self::Substring { start, .. } => start.as_ref(),
            _ => None,
        }
    }

    /// Returns, for `Substring` matches, the fixed string succeeding the last placeholder, if any.
    ///
    /// Returns `None` for all other filter types.
    pub fn substring_end(&self) -> Option<&AssertionValue> {
        match self {
            Self::Substring { end, .. } => end.as_ref(),
            _ => None,
        }
    }

    /// Returns, for `Substring` matches, the fixed strings following each placeholder.
    ///
    /// Returns `None` for all other filter types.
    pub fn substring_middle(&self) -> Option<&[AssertionValue]> {
        match self {
            Self::Substring { middle, .. } => Some(middle),
            _ => None,
        }
    }

    /// Returns, for `Extensible` matches, the matching rule being used, if any has been specified.
    ///
    /// Returns `None` for all other filter types.
    pub fn extensible_matching_rule(&self) -> Option<&AttributeType> {
        match self {
            Self::Extensible { matching_rule, .. } => matching_rule.as_ref(),
            _ => None,
        }
    }

    /// Returns, for `Extensible` matches, whether DN attributes are to be matched as well.
    ///
    /// Returns `None` for all other filter types.
    pub fn extensible_dn_attributes(&self) -> Option<bool> {
        match self {
            Self::Extensible { dn_attributes, .. } => Some(*dn_attributes),
            _ => None,
        }
    }
}
impl fmt::Display for Filter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_extensible_filter() && self.key().is_none() && self.extensible_matching_rule().is_none() {
            // invalid combination
            return Err(fmt::Error);
        }

        write!(f, "(")?;
        match self {
            Filter::And(criteria) => {
                write!(f, "&")?;
                for criterion in criteria {
                    write!(f, "{}", criterion)?;
                }
            },
            Filter::Or(criteria) => {
                write!(f, "|")?;
                for criterion in criteria {
                    write!(f, "{}", criterion)?;
                }
            },
            Filter::Not(criterion) => {
                write!(f, "!{}", criterion)?;
            },
            Filter::Equality { key, value } => {
                write!(f, "{}={}", key, value)?;
            },
            Filter::Substring { key, start, middle, end } => {
                write!(f, "{}=", key)?;
                if let Some(s) = start {
                    write!(f, "{}", s)?;
                }
                for piece in middle {
                    write!(f, "*{}", piece)?;
                }
                if let Some(e) = end {
                    write!(f, "{}", e)?;
                }
            },
            Filter::GreaterOrEqual { key, value } => {
                write!(f, "{}>={}", key, value)?;
            },
            Filter::LessOrEqual { key, value } => {
                write!(f, "{}<={}", key, value)?;
            },
            Filter::Present { key } => {
                write!(f, "{}=*", key)?;
            },
            Filter::Approximate { key, value } => {
                write!(f, "{}~={}", key, value)?;
            },
            Filter::Extensible { matching_rule, key, value, dn_attributes } => {
                if let Some(k) = key {
                    write!(f, "{}", k)?;
                }
                if *dn_attributes {
                    write!(f, ":dn")?;
                }
                if let Some(mr) = matching_rule {
                    write!(f, ":{}", mr)?;
                }
                write!(f, ":={}", value)?;
            },
        }
        write!(f, ")")
    }
}


#[cfg(test)]
mod tests {
    use super::{AttributeDescription, Filter};

    #[test]
    fn test_parse_attribute_description() {
        let attrib = AttributeDescription::try_from_str("cn").unwrap();
        assert_eq!(attrib.attribute_type.as_short(), Some("cn"));
        assert_eq!(attrib.options.len(), 0);

        let attrib = AttributeDescription::try_from_str("2.5.4.3").unwrap();
        assert_eq!(attrib.attribute_type.as_oid().unwrap().len(), 4);
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[0], "2");
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[1], "5");
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[2], "4");
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[3], "3");
        assert_eq!(attrib.options.len(), 0);

        let attrib = AttributeDescription::try_from_str("cn;lang-en").unwrap();
        assert_eq!(attrib.attribute_type.as_short(), Some("cn"));
        assert_eq!(attrib.options.len(), 1);
        assert!(attrib.options.iter().any(|o| o.as_str() == "lang-en"));

        let attrib = AttributeDescription::try_from_str("2.5.4.3;lang-en").unwrap();
        assert_eq!(attrib.attribute_type.as_oid().unwrap().len(), 4);
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[0], "2");
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[1], "5");
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[2], "4");
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[3], "3");
        assert_eq!(attrib.options.len(), 1);
        assert!(attrib.options.iter().any(|o| o.as_str() == "lang-en"));

        let attrib = AttributeDescription::try_from_str("cn;lang-en;pirate-speak").unwrap();
        assert_eq!(attrib.attribute_type.as_short(), Some("cn"));
        assert_eq!(attrib.options.len(), 2);
        assert!(attrib.options.iter().any(|o| o.as_str() == "lang-en"));
        assert!(attrib.options.iter().any(|o| o.as_str() == "pirate-speak"));

        let attrib = AttributeDescription::try_from_str("2.5.4.3;lang-en;pirate-speak").unwrap();
        assert_eq!(attrib.attribute_type.as_oid().unwrap().len(), 4);
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[0], "2");
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[1], "5");
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[2], "4");
        assert_eq!(attrib.attribute_type.as_oid().unwrap()[3], "3");
        assert_eq!(attrib.options.len(), 2);
        assert!(attrib.options.iter().any(|o| o.as_str() == "lang-en"));
        assert!(attrib.options.iter().any(|o| o.as_str() == "pirate-speak"));
    }

    #[test]
    fn test_parse_filter() {
        let filter = Filter::try_from_str("(cn=*)").unwrap();
        assert!(filter.is_present_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short(), Some("cn"));
        assert_eq!(filter.key().unwrap().options.len(), 0);

        let filter = Filter::try_from_str("(cn;lang-en=*)").unwrap();
        assert!(filter.is_present_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short(), Some("cn"));
        assert_eq!(filter.key().unwrap().options.len(), 1);
        assert!(filter.key().unwrap().options.iter().any(|o| o.as_str() == "lang-en"));

        let filter = Filter::try_from_str("(cn;lang-en;pirate-speak=*)").unwrap();
        assert!(filter.is_present_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short(), Some("cn"));
        assert_eq!(filter.key().unwrap().options.len(), 2);
        assert!(filter.key().unwrap().options.iter().any(|o| o.as_str() == "lang-en"));
        assert!(filter.key().unwrap().options.iter().any(|o| o.as_str() == "pirate-speak"));

        let filter = Filter::try_from_str("(2.5.4.3;lang-en;pirate-speak=*)").unwrap();
        assert!(filter.is_present_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap().len(), 4);
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[0], "2");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[1], "5");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[2], "4");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[3], "3");
        assert_eq!(filter.key().unwrap().options.len(), 2);
        assert!(filter.key().unwrap().options.iter().any(|o| o.as_str() == "lang-en"));
        assert!(filter.key().unwrap().options.iter().any(|o| o.as_str() == "pirate-speak"));

        let filter = Filter::try_from_str("(cn=Ondrej Hosek)").unwrap();
        assert!(filter.is_equality_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short(), Some("cn"));
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(cn;lang-en=Ondrej Hosek)").unwrap();
        assert!(filter.is_equality_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short(), Some("cn"));
        assert_eq!(filter.key().unwrap().options.len(), 1);
        assert!(filter.key().unwrap().options.iter().any(|o| o.as_str() == "lang-en"));
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(cn;lang-en;pirate-speak=Admiral Landlubber)").unwrap();
        assert!(filter.is_equality_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short(), Some("cn"));
        assert_eq!(filter.key().unwrap().options.len(), 2);
        assert!(filter.key().unwrap().options.iter().any(|o| o.as_str() == "lang-en"));
        assert!(filter.key().unwrap().options.iter().any(|o| o.as_str() == "pirate-speak"));
        assert_eq!(filter.value().unwrap().as_bytes(), b"Admiral Landlubber");

        let filter = Filter::try_from_str("(monetaryValue>=3)").unwrap();
        assert!(filter.is_greater_or_equal_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short(), Some("monetaryValue"));
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.value().unwrap().as_bytes(), b"3");

        let filter = Filter::try_from_str("(monetaryValue<=3)").unwrap();
        assert!(filter.is_less_or_equal_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short(), Some("monetaryValue"));
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.value().unwrap().as_bytes(), b"3");

        let filter = Filter::try_from_str("(!(monetaryValue<=3))").unwrap();
        assert!(filter.is_not_filter());
        let inner_filter = filter.negated_filter().unwrap();
        assert_eq!(inner_filter.key().unwrap().attribute_type.as_short(), Some("monetaryValue"));
        assert_eq!(inner_filter.key().unwrap().options.len(), 0);
        assert_eq!(inner_filter.value().unwrap().as_bytes(), b"3");

        let filter = Filter::try_from_str("(&(objectClass=booty)(monetaryValue>=4))").unwrap();
        assert!(filter.is_and_filter());
        assert_eq!(filter.subfilters().unwrap().len(), 2);
        let value_filter = filter
            .subfilters().unwrap()
            .iter()
            .filter(|f| f.key().unwrap().attribute_type.as_short().unwrap() == "monetaryValue")
            .nth(0).unwrap();
        assert_eq!(value_filter.key().unwrap().attribute_type.as_short(), Some("monetaryValue"));
        assert_eq!(value_filter.key().unwrap().options.len(), 0);
        assert_eq!(value_filter.value().unwrap().as_bytes(), b"4");
        let class_filter = filter
            .subfilters().unwrap()
            .iter()
            .filter(|f| f.key().unwrap().attribute_type.as_short().unwrap() == "objectClass")
            .nth(0).unwrap();
        assert_eq!(class_filter.key().unwrap().attribute_type.as_short(), Some("objectClass"));
        assert_eq!(class_filter.key().unwrap().options.len(), 0);
        assert_eq!(class_filter.value().unwrap().as_bytes(), b"booty");

        let filter = Filter::try_from_str("(cn=HMS *)").unwrap();
        assert!(filter.is_substring_filter());
        assert_eq!(filter.substring_start().unwrap().as_bytes(), b"HMS ");
        assert_eq!(filter.substring_middle().unwrap().len(), 0);
        assert!(filter.substring_end().is_none());

        let filter = Filter::try_from_str("(cn=HMS *ic)").unwrap();
        assert!(filter.is_substring_filter());
        assert_eq!(filter.substring_start().unwrap().as_bytes(), b"HMS ");
        assert_eq!(filter.substring_middle().unwrap().len(), 0);
        assert_eq!(filter.substring_end().unwrap().as_bytes(), b"ic");

        let filter = Filter::try_from_str("(cn=*ic)").unwrap();
        assert!(filter.is_substring_filter());
        assert!(filter.substring_start().is_none());
        assert_eq!(filter.substring_middle().unwrap().len(), 0);
        assert_eq!(filter.substring_end().unwrap().as_bytes(), b"ic");

        let filter = Filter::try_from_str("(cn=The * of the * by * Smith)").unwrap();
        assert!(filter.is_substring_filter());
        assert_eq!(filter.substring_start().unwrap().as_bytes(), b"The ");
        assert_eq!(filter.substring_middle().unwrap().len(), 2);
        assert_eq!(filter.substring_middle().unwrap()[0].as_bytes(), b" of the ");
        assert_eq!(filter.substring_middle().unwrap()[1].as_bytes(), b" by ");
        assert_eq!(filter.substring_end().unwrap().as_bytes(), b" Smith");

        let filter = Filter::try_from_str("(cn:withoutDiacritics:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short().unwrap(), "cn");
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.extensible_dn_attributes().unwrap(), false);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_short().unwrap(), "withoutDiacritics");
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(2.5.4.3:withoutDiacritics:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap().len(), 4);
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[0], "2");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[1], "5");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[2], "4");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[3], "3");
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.extensible_dn_attributes().unwrap(), false);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_short().unwrap(), "withoutDiacritics");
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(cn:1.1:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short().unwrap(), "cn");
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.extensible_dn_attributes().unwrap(), false);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap().len(), 2);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap()[0], "1");
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap()[1], "1");
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(2.5.4.3:1.1:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap().len(), 4);
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[0], "2");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[1], "5");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[2], "4");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[3], "3");
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.extensible_dn_attributes().unwrap(), false);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap().len(), 2);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap()[0], "1");
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap()[1], "1");
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(2.5.4.3:dn:1.1:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap().len(), 4);
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[0], "2");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[1], "5");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[2], "4");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[3], "3");
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.extensible_dn_attributes().unwrap(), true);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap().len(), 2);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap()[0], "1");
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap()[1], "1");
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(cn:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short().unwrap(), "cn");
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.extensible_dn_attributes().unwrap(), false);
        assert!(filter.extensible_matching_rule().is_none());
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(2.5.4.3:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap().len(), 4);
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[0], "2");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[1], "5");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[2], "4");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[3], "3");
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.extensible_dn_attributes().unwrap(), false);
        assert!(filter.extensible_matching_rule().is_none());
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(:1.1:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert!(filter.key().is_none());
        assert_eq!(filter.extensible_dn_attributes().unwrap(), false);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap().len(), 2);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap()[0], "1");
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap()[1], "1");
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(:dn:withoutDiacritics:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert!(filter.key().is_none());
        assert_eq!(filter.extensible_dn_attributes().unwrap(), true);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_short().unwrap(), "withoutDiacritics");
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(cn:dn:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_short().unwrap(), "cn");
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.extensible_dn_attributes().unwrap(), true);
        assert!(filter.extensible_matching_rule().is_none());
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(2.5.4.3:dn:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap().len(), 4);
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[0], "2");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[1], "5");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[2], "4");
        assert_eq!(filter.key().unwrap().attribute_type.as_oid().unwrap()[3], "3");
        assert_eq!(filter.key().unwrap().options.len(), 0);
        assert_eq!(filter.extensible_dn_attributes().unwrap(), true);
        assert!(filter.extensible_matching_rule().is_none());
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(:dn:withoutDiacritics:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert!(filter.key().is_none());
        assert_eq!(filter.extensible_dn_attributes().unwrap(), true);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_short().unwrap(), "withoutDiacritics");
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        let filter = Filter::try_from_str("(:dn:1.1:=Ondrej Hosek)").unwrap();
        assert!(filter.is_extensible_filter());
        assert!(filter.key().is_none());
        assert_eq!(filter.extensible_dn_attributes().unwrap(), true);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap().len(), 2);
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap()[0], "1");
        assert_eq!(filter.extensible_matching_rule().unwrap().as_oid().unwrap()[1], "1");
        assert_eq!(filter.value().unwrap().as_bytes(), b"Ondrej Hosek");

        // neither attribute nor matcher type
        assert_eq!(Filter::try_from_str("(:=Ondrej Hosek)"), None);
        assert_eq!(Filter::try_from_str("(:dn:=Ondrej Hosek)"), None);
    }
}
