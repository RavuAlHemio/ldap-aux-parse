//! Functionality related to parsing LDAP filters.
//!
//! The string representation of LDAP filters is defined in RFC4515.


use std::collections::BTreeSet;


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
    EqualityMatch { key: String, value: Vec<u8> },

    /// A criterion which matches when the attribute identified by the key matches the given
    /// pattern.
    ///
    /// To visualize the filter, between each string in the pattern, imagine a placeholder that can
    /// represent any sequence of bytes, including an empty one.
    Substring { key: String, start: Option<Vec<u8>>, middle: Vec<Vec<u8>>, end: Option<Vec<u8>> },

    /// A criterion which matches when the value of the attribute identified by the key is greater
    /// than or equal to the provided value.
    GreaterOrEqualMatch { key: String, value: Vec<u8> },

    /// A criterion which matches when the value of the attribute identified by the key is smaller
    /// than or equal to the provided value.
    LessOrEqualMatch { key: String, value: Vec<u8> },

    /// A criterion which matches when the attribute identified by the key has at least one value.
    Present { key: String },

    /// A criterion which matches when the value of the attribute identified by the key is
    /// approximately equal to the provided value.
    Approximate { key: String, value: Vec<u8> },

    /// A specialized criterion.
    Extensible {
        matching_rule: Option<String>,
        key: Option<String>,
        value: Vec<u8>,
        dn_attributes: bool,
    },
}
