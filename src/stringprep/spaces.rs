//! Dealing with insignificant spaces when preparing strings for comparison.


use unicode_properties::{GeneralCategoryGroup, UnicodeGeneralCategory};


/// Finds the first character in `s` that matches `predicate` and is not followed by a combining
/// mark.
///
/// A combining mark is a Unicode character with General_Category `M`.
///
/// The function returns the byte index of the matched character, not of the mark following it.
///
/// The function panics if a predicate is passed that matches a combining mark.
pub(crate) fn find_next_unmarked_character<F: FnMut(char) -> bool>(s: &str, mut predicate: F) -> Option<usize> {
    let mut find_start_index = 0;
    while let Some(relative_char_index) = s[find_start_index..].find(&mut predicate) {
        let char_index = find_start_index + relative_char_index;
        let mut char_iter = s[char_index..].char_indices();
        let (_zero, matched_char) = char_iter.next().unwrap();
        if matched_char.general_category_group() == GeneralCategoryGroup::Mark {
            panic!("predicate matched combining mark");
        }

        match char_iter.next() {
            Some((possible_mark_index, possible_mark_char)) => {
                // is it a mark?
                if possible_mark_char.general_category_group() == GeneralCategoryGroup::Mark {
                    // alright, keep looking
                    find_start_index = char_index + possible_mark_index;
                    continue;
                } else {
                    // no -- we matched an unmarked character
                    return Some(char_index);
                }
            },
            None => {
                // matched character at end of string => unmarked
                return Some(char_index);
            },
        }
    }

    None
}


/// An iterator which splits a string slice into substrings delimited by spaces followed by
/// combining marks.
///
/// A space is the Unicode character U+0020. A combining mark is a Unicode character with
/// General_Category `M`.
/// 
/// Each slice returned by the iterator does not contain the space character but does contain the
/// combining mark. The original string can be reconstructed by interspersing space (U+0020)
/// characters between each pair of slices.
///
/// This iterator is useful for implementing RFC4518 § 2.6.1. Since the Normalize step (§ 2.3)
/// transforms the string into Unicode Normalization Form KC, any spacing modifier symbols, such as
/// U+02D8 BREVE, have become sequences of U+0020 SPACE and the corresponding combining mark (U+0306
/// COMBINING BREVE in this example) and thus must be treated differently from U+0020 SPACE in other
/// contexts.
pub(crate) struct SplitAtMarkedSpace<'a> {
    s: &'a str,
    pos: usize,
}
impl<'a> SplitAtMarkedSpace<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            s,
            pos: 0,
        }
    }
}
impl<'a> Iterator for SplitAtMarkedSpace<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        const SPACE: char = ' ';

        if self.pos > self.s.len() {
            return None;
        }

        let mut next_space_search_offset = self.pos;
        while next_space_search_offset < self.s.len() {
            let next_space_index = match self.s[next_space_search_offset..].find(SPACE) {
                None => {
                    // no more spaces; return the rest of the string
                    let rest_slice = &self.s[self.pos..];

                    // never return another piece
                    self.pos = self.s.len() + 1;
                    return Some(rest_slice);
                },
                Some(nsi) => next_space_search_offset + nsi,
            };

            // check character after space
            let char_after_space = match self.s[next_space_index..].chars().nth(1) {
                Some(cas) => cas,
                None => {
                    // space at the end of the string
                    // return the rest of the string and that's it
                    let rest_slice = &self.s[self.pos..];
                    self.pos = self.s.len() + 1;
                    return Some(rest_slice);
                },
            };

            if char_after_space.general_category_group() == GeneralCategoryGroup::Mark {
                // perfect

                // return the slice until the space
                let until_slice = &self.s[self.pos..next_space_index];

                // next slice starts at the combining mark, right after the space
                self.pos = next_space_index + SPACE.len_utf8();

                return Some(until_slice);
            } else {
                // a space followed by something that's not a combining mark
                // run the loop again
                next_space_search_offset = next_space_index + SPACE.len_utf8();
            }
        }

        // no more marked space found
        // return the rest of the string and that's it
        let rest_slice = &self.s[self.pos..];
        self.pos = self.s.len() + 1;
        Some(rest_slice)
    }
}


#[cfg(test)]
mod tests {
    use super::{find_next_unmarked_character, SplitAtMarkedSpace};

    #[test]
    fn test_find_next_unmarked_character() {
        assert_eq!(find_next_unmarked_character("", |c| c == ' '), None);
        assert_eq!(find_next_unmarked_character(" ", |c| c == ' '), Some(0));
        assert_eq!(find_next_unmarked_character(" \u{301}", |c| c == ' '), None);
        assert_eq!(find_next_unmarked_character(" \u{301} ", |c| c == ' '), Some(3));
        assert_eq!(find_next_unmarked_character(" \u{301} \u{301}\u{301} ", |c| c == ' '), Some(8));
        assert_eq!(find_next_unmarked_character(" \u{301} \u{301}\u{301} a", |c| c == ' '), Some(8));
        assert_eq!(find_next_unmarked_character(" \u{301} \u{301}\u{301} a\u{301}", |c| c == ' '), Some(8));
        assert_eq!(find_next_unmarked_character(" \u{301} \u{301}\u{301} \u{301}", |c| c == ' '), None);
    }

    macro_rules! assert_keeps_returning_none {
        ($iterator:expr) => {
            assert_eq!($iterator.next(), None);
            assert_eq!($iterator.next(), None);
            assert_eq!($iterator.next(), None);
        };
    }

    #[test]
    fn test_split_at_marked_space() {
        let mut sams = SplitAtMarkedSpace::new("");
        assert_eq!(sams.next(), Some(""));
        assert_keeps_returning_none!(sams);

        let mut sams = SplitAtMarkedSpace::new("abc");
        assert_eq!(sams.next(), Some("abc"));
        assert_keeps_returning_none!(sams);

        let mut sams = SplitAtMarkedSpace::new("one two three");
        assert_eq!(sams.next(), Some("one two three"));
        assert_keeps_returning_none!(sams);

        let mut sams = SplitAtMarkedSpace::new("one \u{301}two three");
        assert_eq!(sams.next(), Some("one"));
        assert_eq!(sams.next(), Some("\u{301}two three"));
        assert_keeps_returning_none!(sams);

        let mut sams = SplitAtMarkedSpace::new("one \u{301}two \u{301}three");
        assert_eq!(sams.next(), Some("one"));
        assert_eq!(sams.next(), Some("\u{301}two"));
        assert_eq!(sams.next(), Some("\u{301}three"));
        assert_keeps_returning_none!(sams);

        let mut sams = SplitAtMarkedSpace::new(" \u{301}one \u{301}two \u{301}three");
        assert_eq!(sams.next(), Some(""));
        assert_eq!(sams.next(), Some("\u{301}one"));
        assert_eq!(sams.next(), Some("\u{301}two"));
        assert_eq!(sams.next(), Some("\u{301}three"));
        assert_keeps_returning_none!(sams);

        let mut sams = SplitAtMarkedSpace::new(" \u{301}one \u{301}two \u{301}three \u{301}");
        assert_eq!(sams.next(), Some(""));
        assert_eq!(sams.next(), Some("\u{301}one"));
        assert_eq!(sams.next(), Some("\u{301}two"));
        assert_eq!(sams.next(), Some("\u{301}three"));
        assert_eq!(sams.next(), Some("\u{301}"));
        assert_keeps_returning_none!(sams);

        let mut sams = SplitAtMarkedSpace::new("\u{301}one \u{301}two \u{301}three \u{301}");
        // starts with a mark without a space before it!
        assert_eq!(sams.next(), Some("\u{301}one"));
        assert_eq!(sams.next(), Some("\u{301}two"));
        assert_eq!(sams.next(), Some("\u{301}three"));
        assert_eq!(sams.next(), Some("\u{301}"));
        assert_keeps_returning_none!(sams);

        let mut sams = SplitAtMarkedSpace::new("\u{301}one\u{301}two \u{301}three\u{301}");
        // more marks without spaces
        assert_eq!(sams.next(), Some("\u{301}one\u{301}two"));
        assert_eq!(sams.next(), Some("\u{301}three\u{301}"));
        assert_keeps_returning_none!(sams);
    }
}
