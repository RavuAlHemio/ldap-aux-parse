//! String preparation for comparison according to RFC4518.


mod case_fold_map;
mod case_sensitive_map;
mod mapping;


use std::borrow::Cow;
use std::cmp::Ordering;

use unicode_normalization::UnicodeNormalization;

use crate::stringprep::mapping::{Mapping, MappingTarget};


// we accept &str, which means the Transcode step is left to the application


/// Performs the Map step of string preparation.
fn map(s: &str, fold_case: bool) -> Cow<str> {
    fn get_target(sorted_mappings: &[Mapping], needle: char) -> Option<&MappingTarget> {
        sorted_mappings.binary_search_by(|mapping| {
            if mapping.first_char <= needle && needle <= mapping.last_char {
                Ordering::Equal
            } else if mapping.first_char > needle {
                // range is greater than the needle
                Ordering::Greater
            } else {
                assert!(mapping.last_char < needle);
                // range is less than the needle
                Ordering::Less
            }
        })
            .ok()
            .map(|i| &sorted_mappings[i].target)
    }

    let want_mappings = if fold_case {
        &crate::stringprep::case_fold_map::MAPPING[..]
    } else {
        &crate::stringprep::case_sensitive_map::MAPPING[..]
    };

    // check if there is anything to map
    let map_anything = s.chars()
        .any(|c|
            get_target(want_mappings, c)
                .map(|mt| mt.will_change(c))
                .unwrap_or(false)
        );
    if !map_anything {
        return Cow::Borrowed(s)
    }

    let mut output = String::with_capacity(s.len());
    for c in s.chars() {
        if let Some(target) = get_target(want_mappings, c) {
            target.map_write(c, &mut output);
        } else {
            output.push(c);
        }
    }

    Cow::Owned(output)
}


/// Performs the Normalize step of string preparation.
fn normalize(s: &str) -> Cow<str> {
    if s.chars().eq(s.nfkc()) {
        // no normalization required
        return Cow::Borrowed(s);
    }

    let mut ret = String::with_capacity(s.len());
    for c in s.nfkc() {
        ret.push(c);
    }
    Cow::Owned(ret)
}


/// Performs the Prohibit step of string preparation.
fn is_prohibited(s: &str) -> bool {
    todo!();
}
