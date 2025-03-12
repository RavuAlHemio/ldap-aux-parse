//! String preparation for comparison according to RFC4518.


mod case_fold_map;
mod case_sensitive_map;
mod mapping;
mod prohibit_map;
mod spaces;


use std::borrow::Cow;
use std::cmp::Ordering;

use spaces::SplitAtMarkedSpace;
use unicode_normalization::UnicodeNormalization;

use crate::stringprep::mapping::{Mapping, MappingTarget};


// we accept &str, which means the Transcode step is left to the application


/// Obtains the mapping target for the given character in the given sorted list of mappings.
fn map_get_target(sorted_mappings: &[Mapping], needle: char) -> Option<&MappingTarget> {
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


/// Performs the Map step of string preparation.
fn map(s: &str, fold_case: bool) -> Cow<str> {
    let want_mappings = if fold_case {
        &crate::stringprep::case_fold_map::MAPPING[..]
    } else {
        &crate::stringprep::case_sensitive_map::MAPPING[..]
    };

    // check if there is anything to map
    let map_anything = s.chars()
        .any(|c|
            map_get_target(want_mappings, c)
                .map(|mt| mt.will_change(c))
                .unwrap_or(false)
        );
    if !map_anything {
        return Cow::Borrowed(s)
    }

    let mut output = String::with_capacity(s.len());
    for c in s.chars() {
        if let Some(target) = map_get_target(want_mappings, c) {
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
    let prohibit_mapping = &crate::stringprep::prohibit_map::MAPPING;
    for c in s.chars() {
        if map_get_target(prohibit_mapping, c).is_some() {
            return true;
        }
    }
    false
}


// the Check bidi step is skipped, as RFC4518 says "Bidirectional characters are ignored."
// (at least, I hope that's what RFC4518 means by that)


/// Performs the Insignificant Character Handling step for full strings matched using case-ignore
/// or exact-string matching.
fn handle_insignificant_spaces_full(s: &str) -> Cow<str> {
    // place spaces followed by combining marks beyond consideration
    let mut marked_space_pieces: Vec<String> = SplitAtMarkedSpace::new(s)
        .map(|s| s.to_owned())
        .collect();

    // double each space
    for piece in &mut marked_space_pieces {
        *piece = piece.replace(" ", "  ");
    }

    // fold multiples of spaces down to two
    for piece in &mut marked_space_pieces {
        while let Some(many_spaces_index) = piece.find("   ") {
            // how far does that go?
            let many_spaces_length = piece[many_spaces_index..]
                .find(|c| c != ' ')
                .unwrap_or(piece[many_spaces_index..].len());
            let many_spaces_end = many_spaces_index + many_spaces_length;

            // reduce that to two spaces
            piece.replace_range(
                many_spaces_index..many_spaces_end,
                "  ",
            );
        }
    }

    // squeeze leading spaces into one
    let first_piece = marked_space_pieces.first_mut().unwrap();
    match first_piece.find(|c| c != ' ') {
        Some(first_non_space) => first_piece.replace_range(..first_non_space, " "),
        None => first_piece.replace_range(.., " "),
    };

    // squeeze trailing spaces into one
    let last_piece = marked_space_pieces.last_mut().unwrap();
    match last_piece.rfind(|c| c != ' ') {
        Some(last_non_space) => {
            let lns_length = last_piece[last_non_space..].chars().nth(0).unwrap().len_utf8();
            last_piece.replace_range(last_non_space+lns_length.., " ");
        },
        None => {
            // it's all spaces
            last_piece.replace_range(.., " ");
        },
    }

    // if we are left with only one space, ensure we leave with two
    if marked_space_pieces.len() == 1 && marked_space_pieces[0] == " " {
        marked_space_pieces[0].push(' ');
    }

    // glue it all back together
    let output: String = marked_space_pieces.join(" ");
    Cow::Owned(output)
}


/// The location where the given substring is being matched.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum SubstringLocation {
    /// The substring is matching at the beginning of the string, e.g. the `abc` in `abc*def*ghi`.
    Initial,

    /// The substring is matching somewhere within the string, e.g. the `def` in `abc*def*ghi` or `*def*`.
    Any,

    /// The substring is matching at the end of the string, e.g. the `ghi` in `abc*def*ghi`.
    Final,
}


/// Performs the Insignificant Character Handling step for parts of a substring-match filter using
/// case-ignore or exact-string matching.
fn handle_insignificant_spaces_substring(s: &str, location: SubstringLocation) -> Cow<str> {
    let mut ret = s.to_owned();

    if location == SubstringLocation::Initial {
        let first_nonspace = ret
            .char_indices()
            .filter(|(_i, c)| *c != ' ')
            .map(|(i, _c)| i)
            .nth(0);
        if let Some(fns) = first_nonspace {
            if fns == 0 {
                // insert a space at the beginning
                ret.insert(0, ' ');
            } else {
                
            }
        }
    }

    todo!();
}

/// Performs the Insignificant Character Handling step for strings matched using case-ignore and
/// exact-string matching.
fn handle_numeric_string_insignificant_characters(s: &str) -> Cow<str> {
    todo!();
}


/// Performs the Insignificant Character Handling step for strings matched using telephoneNumber
/// matching.
fn handle_telephone_number_insignificant_characters(s: &str) -> Cow<str> {
    todo!();
}
