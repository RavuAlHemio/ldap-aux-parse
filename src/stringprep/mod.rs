//! String preparation for comparison according to RFC4518.


mod case_fold_map;
mod case_sensitive_map;
mod mapping;
mod prohibit_map;
mod spaces;


use std::borrow::Cow;
use std::cmp::Ordering;

use unicode_normalization::UnicodeNormalization;
use unicode_properties::{GeneralCategoryGroup, UnicodeGeneralCategory};

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
    // > For the purposes of this section, a space is defined to be the SPACE (U+0020) code point
    // > followed by no combining marks.

    // > If the input string contains no non-space character, then the output is exactly two SPACEs.
    // > Otherwise, [...] the string starts with exactly one space character, ends with exactly one
    // > SPACE character, and any inner (non-empty) sequence of space characters is replaced with
    // > exactly two SPACE characters.

    // we do this using the following state machine:
    //
    // digraph {
    //     Start -> StartSpace [label="s @ s"];
    //     Start -> End [label="t @ ss"];
    //     Start -> Char [label="o @ so"];
    //     Start -> Char [label="c @ sc"];
    //
    //     StartSpace -> End [label="t @ s"];
    //     StartSpace -> Char [label="c @ sc"];
    //     StartSpace -> Char [label="o @ o"];
    //     StartSpace -> StartSpace2 [label="s @ e"];
    //
    //     StartSpace2 -> StartSpace2 [label="s @ e"];
    //     StartSpace2 -> End [label="t @ s"];
    //     StartSpace2 -> Char [label="c @ sc"];
    //     StartSpace2 -> Char [label="o @ o"];
    //
    //     Char -> Char [label="o @ o"];
    //     Char -> Char [label="c @ c"];
    //     Char -> End [label="t @ s"];
    //     Char -> Space [label="s @ s"];
    //
    //     Space -> Char [label="o @ so"];
    //     Space -> Char [label="c @ c"];
    //     Space -> End [label="t @ e"];
    //     Space -> Space2 [label="s @ e"];
    //
    //     Space2 -> Space2 [label="s @ e"];
    //     Space2 -> Char [label="o @ so"];
    //     Space2 -> Char [label="c @ ssc"];
    //     Space2 -> End [label="t @ e"];
    //
    //     End [shape="doublecircle",label=""];
    // }
    //
    // where the letters mean the following:
    // s = space character
    // c = combining mark
    // o = any other character
    // e = epsilon (output nothing)
    // t = terminator (virtual character meaning end-of-input)

    #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    enum ParserState {
        #[default] Start,
        StartSpace,
        StartSpace2,
        Char,
        Space,
        Space2,
    }

    #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    enum CharacterClass {
        Space,
        CombiningMark(char),
        Other(char),
        Terminator,
    }
    impl CharacterClass {
        pub fn from_char_opt(c: Option<char>) -> Self {
            if let Some(cc) = c {
                if cc == ' ' {
                    Self::Space
                } else if cc.general_category_group() == GeneralCategoryGroup::Mark {
                    Self::CombiningMark(cc)
                } else {
                    Self::Other(cc)
                }
            } else {
                Self::Terminator
            }
        }
    }

    let mut state = ParserState::Start;
    let mut output = String::new();
    let mut char_iter = s.chars();
    loop {
        let c = char_iter.next();
        let c_class = CharacterClass::from_char_opt(c);

        match (state, c_class) {
            (ParserState::Start, CharacterClass::CombiningMark(m)) => {
                output.push(' ');
                output.push(m);
                state = ParserState::Char;
            },
            (ParserState::Start, CharacterClass::Other(o)) => {
                output.push(' ');
                output.push(o);
                state = ParserState::Char;
            },
            (ParserState::Start, CharacterClass::Space) => {
                output.push(' ');
                state = ParserState::StartSpace;
            },
            (ParserState::Start, CharacterClass::Terminator) => {
                output.push_str("  ");
                break;
            },

            (ParserState::StartSpace, CharacterClass::CombiningMark(m)) => {
                output.push(' ');
                output.push(m);
                state = ParserState::Char;
            },
            (ParserState::StartSpace, CharacterClass::Other(o)) => {
                output.push(o);
                state = ParserState::Char;
            },
            (ParserState::StartSpace, CharacterClass::Space) => {
                state = ParserState::StartSpace2;
            },
            (ParserState::StartSpace, CharacterClass::Terminator) => {
                output.push(' ');
                break;
            },

            (ParserState::StartSpace2, CharacterClass::CombiningMark(m)) => {
                output.push(' ');
                output.push(m);
                state = ParserState::Char;
            },
            (ParserState::StartSpace2, CharacterClass::Other(o)) => {
                output.push(o);
                state = ParserState::Char;
            },
            (ParserState::StartSpace2, CharacterClass::Space) => {
                // no change...
            },
            (ParserState::StartSpace2, CharacterClass::Terminator) => {
                output.push(' ');
                break;
            },

            (ParserState::Char, CharacterClass::CombiningMark(m)) => {
                output.push(m);
            },
            (ParserState::Char, CharacterClass::Other(o)) => {
                output.push(o);
            },
            (ParserState::Char, CharacterClass::Space) => {
                output.push(' ');
                state = ParserState::Space;
            },
            (ParserState::Char, CharacterClass::Terminator) => {
                output.push(' ');
                break;
            },

            (ParserState::Space, CharacterClass::CombiningMark(m)) => {
                output.push(m);
                state = ParserState::Char;
            },
            (ParserState::Space, CharacterClass::Other(o)) => {
                output.push(' ');
                output.push(o);
                state = ParserState::Char;
            },
            (ParserState::Space, CharacterClass::Space) => {
                state = ParserState::Space2;
            },
            (ParserState::Space, CharacterClass::Terminator) => {
                break;
            },

            (ParserState::Space2, CharacterClass::CombiningMark(m)) => {
                output.push(' ');
                output.push(' ');
                output.push(m);
                state = ParserState::Char;
            },
            (ParserState::Space2, CharacterClass::Other(o)) => {
                output.push(' ');
                output.push(o);
                state = ParserState::Char;
            },
            (ParserState::Space2, CharacterClass::Space) => {
                // no change...
            },
            (ParserState::Space2, CharacterClass::Terminator) => {
                break;
            },
        }
    }

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


#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use unicode_properties::GeneralCategoryGroup;

    use super::handle_insignificant_spaces_full;

    fn naive_handle_insignificant_spaces(s: &str) -> String {
        use unicode_properties::UnicodeGeneralCategory;

        const ONE_SPACE: &str = " ";
        const TWO_SPACES: &str = "  ";
        const THREE_SPACES: &str = "   ";

        // empty string => "  "
        if s.len() == 0 {
            return TWO_SPACES.to_owned();
        }

        // find all spaces followed by a combining mark and replace them with U+0000
        // (a character that is stripped out from the input)
        let mut string = s.to_owned();

        loop {
            let mut space_comb_index = None;
            let mut prev_index_char = None;
            for (i, c) in string.char_indices() {
                if let Some((pi, pc)) = prev_index_char {
                    if pc == ' ' && c.general_category_group() == GeneralCategoryGroup::Mark {
                        // yup!
                        space_comb_index = Some(pi);
                    }
                }
                prev_index_char = Some((i, c));
            }

            if let Some(sci) = space_comb_index {
                string.replace_range(sci..sci+1, "\u{0000}");
            } else {
                // all spaces followed by combining marks have been handled
                break;
            }
        }

        // double all remaining spaces
        string = string.replace(ONE_SPACE, TWO_SPACES);

        // iteratively reduce triplets of spaces into pairs
        loop {
            let shorter_string = string.replace(THREE_SPACES, TWO_SPACES);
            if shorter_string.len() >= string.len() {
                // nothing more to reduce
                break;
            }
            string = shorter_string;
        }

        // reduce initial spaces to one
        while string.starts_with(TWO_SPACES) {
            string.replace_range(0..1, "");
        }
        if !string.starts_with(ONE_SPACE) {
            string.insert(0, ' ');
        }

        // reduce final spaces to one
        while string.ends_with(TWO_SPACES) {
            string.pop();
        }
        if !string.ends_with(ONE_SPACE) {
            string.push(' ');
        }

        // if we only have spaces, ensure we have two
        if string == " " {
            string.push(' ');
        }

        // transform U+0000s back into spaces
        string = string.replace("\u{0000}", ONE_SPACE);

        string
    }

    #[test]
    fn test_handle_insignificant_spaces_full() {
        let mut queue = VecDeque::new();
        // combining mark, "other character", space
        const POOL: [char; 3] = ['\u{301}', 'O', ' '];
        const TARGET_LENGTH: usize = 12;
        queue.push_back(String::new());

        while let Some(item) = queue.pop_front() {
            if item.len() < TARGET_LENGTH {
                // create new entries by appending each of POOL in turn
                for choice in POOL {
                    let mut new_item = item.clone();
                    new_item.push(choice);
                    queue.push_back(new_item);
                }
            } else {
                // try it out
                assert_eq!(
                    handle_insignificant_spaces_full(&item),
                    naive_handle_insignificant_spaces(&item),
                    "input is {:?}",
                    item,
                );
            }
        }
    }
}
