pub mod dn;


/// Finds the first occurrence of a byte in a string starting at a specific index.
///
/// Slices the string from `start_byte_index` forward, then takes care of adding `start_byte_index`
/// to the returned value.
pub(crate) fn find_next_index(haystack: &[u8], needle: u8, start_byte_index: usize) -> Option<usize> {
    haystack[start_byte_index..].iter().position(|b| *b == needle)
        .map(|i| i + start_byte_index)
}


/// Part of a byte string that is either an unescaped slice or a single byte derived from an escape
/// sequence.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum ByteStringPart<'a> {
    /// A slice of unescaped bytes.
    UnescapedSlice { index: usize, slice: &'a [u8] },

    /// A single byte derived from an escape sequence.
    ///
    /// The index points at the escape character preceding `byte`.
    EscapedByte { index: usize, byte: u8 },
}
impl<'a> ByteStringPart<'a> {
    /// Returns `Some(slice)` if this `ByteStringPart` is an `UnescapedSlice { slice, .. }` and
    /// `None` otherwise.
    pub fn as_unescaped(&self) -> Option<&[u8]> {
        match self {
            Self::UnescapedSlice { slice, .. } => Some(slice),
            _ => None,
        }
    }

    /// Returns `Some(byte)` if this `ByteStringPart` is an `EscapedByte { byte, .. }` and `None`
    /// otherwise.
    pub fn as_escaped(&self) -> Option<u8> {
        match self {
            Self::EscapedByte { byte, .. } => Some(*byte),
            _ => None,
        }
    }

    /// Returns the index of this `ByteStringPart`.
    pub fn index(&self) -> usize {
        match self {
            Self::EscapedByte { index, .. } => *index,
            Self::UnescapedSlice { index, .. } => *index,
        }
    }

    /// Returns the length, in bytes, of this `ByteStringPart`.
    pub fn len(&self) -> usize {
        match self {
            Self::UnescapedSlice { slice, .. } => slice.len(),
            Self::EscapedByte { .. } => 1,
        }
    }
}

pub(crate) trait FromByteStringParts<'a> {
    type Error;
    fn from_byte_string_parts(parts: &[ByteStringPart<'a>]) -> Result<Self, Self::Error> where Self : Sized;
}

/// Splits a slice of byte string parts into multiple substrings separated by the given byte.
///
/// The needle (separator byte) is only recognized within unescaped slices, not as an escaped byte.
/// Thus, escaping the needle allows it to be "smuggled" into values without it counting as a
/// separator.
///
/// Supplying `max_count` makes the splitting stop after that many separators have been encountered.
/// Pass 0 as `max_count` to split the slice at all separators.
///
/// As an example, we take the following distinguished name:
///
/// `o=Dewey\, Cheatham & Howe,l=London,c=GB`
///
/// Using [`process_dn_escapes`](crate::dn::process_dn_escapes), it is split into the following byte
/// string parts:
///
/// ```plain
/// [
///     UnescapedSlice(b"o=Dewey"),
///     EscapedByte(b','),
///     UnescapedSlice(b" Cheatham & Howe,l=London,c=GB"),
/// ]
/// ```
///
/// Calling [`split_byte_string_parts_at`] with a needle of `b','` returns the following parts:
///
/// ```plain
/// [
///     [
///         UnescapedSlice(b"o=Dewey"),
///         EscapedByte(b','),
///         UnescapedSlice(b" Cheatham & Howe"),
///     ],
///     [
///         UnescapedSlice(b"l=London"),
///     ],
///     [
///         UnescapedSlice(b"c=GB"),
///     ],
/// ]
/// ```
pub(crate) fn split_byte_string_parts_at<'a>(haystack: &[ByteStringPart<'a>], needle: u8, max_count: usize) -> Vec<Vec<ByteStringPart<'a>>> {
    let mut slices = Vec::new();
    let mut current_slice = Vec::new();
    let mut split_count = 0;
    for bale in haystack {
        if max_count > 0 && split_count >= max_count {
            // enough splitting, just add it to the current slice
            current_slice.push(bale.clone());
            continue;
        }

        match bale {
            ByteStringPart::UnescapedSlice { index, slice } => {
                let mut start_index = 0;
                while let Some(needle_location) = find_next_index(slice, needle, start_index) {
                    let slice_before_needle = &slice[start_index..needle_location];
                    if slice_before_needle.len() > 0 {
                        current_slice.push(ByteStringPart::UnescapedSlice {
                            index: *index + start_index,
                            slice: slice_before_needle,
                        });
                    }

                    let swap_slice = std::mem::replace(&mut current_slice, Vec::new());
                    slices.push(swap_slice);

                    let needle_length = 1;
                    start_index = needle_location + needle_length;

                    split_count += 1;
                    if max_count > 0 && split_count >= max_count {
                        // stop searching for more needles, append the rest
                        break;
                    }
                }

                // push the rest
                let final_slice = &slice[start_index..];
                if final_slice.len() > 0 {
                    current_slice.push(ByteStringPart::UnescapedSlice {
                        index: *index + start_index,
                        slice: final_slice,
                    });
                }
            },
            ByteStringPart::EscapedByte { .. } => {
                current_slice.push(bale.clone());
            },
        }
    }
    if current_slice.len() > 0 {
        slices.push(current_slice);
    }
    slices
}


/// A slice of a textual or binary string.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum StringSlice<'a> {
    /// A slice that is valid UTF-8.
    Textual { index: usize, string: &'a str },

    /// A byte that is not part of a valid UTF-8 sequence.
    Byte { index: usize, byte: u8 },
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


#[cfg(test)]
mod tests {
    use super::{ByteStringPart, split_byte_string_parts_at};

    #[test]
    fn test_split() {
        let haystack = vec![
            ByteStringPart::UnescapedSlice { slice: b"o=Dewey", index: 0 },
            ByteStringPart::EscapedByte { byte: b',', index: 7 },
            ByteStringPart::UnescapedSlice { slice: b" Cheatham & Howe,l=Lo", index: 9 },
            ByteStringPart::EscapedByte { byte: b'n', index: 30 },
            ByteStringPart::UnescapedSlice { slice: b"don,c=GB", index: 32 },
        ];

        let chunks = split_byte_string_parts_at(&haystack, b',', 0);
        assert_eq!(chunks.len(), 3);
        assert_eq!(chunks[0].len(), 3);
        assert_eq!(chunks[0][0], ByteStringPart::UnescapedSlice { index: 0, slice: b"o=Dewey" });
        assert_eq!(chunks[0][1], ByteStringPart::EscapedByte { index: 7, byte: b',' });
        assert_eq!(chunks[0][2], ByteStringPart::UnescapedSlice { index: 9, slice: b" Cheatham & Howe" });
        assert_eq!(chunks[1].len(), 3);
        assert_eq!(chunks[1][0], ByteStringPart::UnescapedSlice { index: 26, slice: b"l=Lo" });
        assert_eq!(chunks[1][1], ByteStringPart::EscapedByte { index: 30, byte: b'n' });
        assert_eq!(chunks[1][2], ByteStringPart::UnescapedSlice { index: 32, slice: b"don" });
        assert_eq!(chunks[2].len(), 1);
        assert_eq!(chunks[2][0], ByteStringPart::UnescapedSlice { index: 36, slice: b"c=GB" });

        let chunks = split_byte_string_parts_at(&haystack, b',', 1);
        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0].len(), 3);
        assert_eq!(chunks[0][0], ByteStringPart::UnescapedSlice { index: 0, slice: b"o=Dewey" });
        assert_eq!(chunks[0][1], ByteStringPart::EscapedByte { index: 7, byte: b',' });
        assert_eq!(chunks[0][2], ByteStringPart::UnescapedSlice { index: 9, slice: b" Cheatham & Howe" });
        assert_eq!(chunks[1].len(), 3);
        assert_eq!(chunks[1][0], ByteStringPart::UnescapedSlice { index: 26, slice: b"l=Lo" });
        assert_eq!(chunks[1][1], ByteStringPart::EscapedByte { index: 30, byte: b'n' });
        assert_eq!(chunks[1][2], ByteStringPart::UnescapedSlice { index: 32, slice: b"don,c=GB" });
    }
}
