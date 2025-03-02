pub mod dn;


/// Finds the first occurrence of a character in a string starting at a specific index.
///
/// Slices the string from `start_byte_index` forward, then takes care of adding `start_byte_index`
/// to the returned value.
pub(crate) fn find_next_index(haystack: &str, needle: char, start_byte_index: usize) -> Option<usize> {
    haystack[start_byte_index..].find(needle)
        .map(|i| i + start_byte_index)
}


/// Part of a byte string that is either an unescaped slice or a single byte derived from an escape
/// sequence.
pub(crate) enum ByteStringPart<'a> {
    /// A slice of unescaped bytes.
    UnescapedSlice { index: usize, slice: &'a [u8] },

    /// A single byte derived from an escape sequence.
    EscapedByte { index: usize, byte: u8 },
}
impl<'a> ByteStringPart<'a> {
    /// Returns `Some(slice)` if this `ByteStringPart` is an `UnescapedSlice(slice)` and `None`
    /// otherwise.
    pub fn as_unescaped(&self) -> Option<&[u8]> {
        match self {
            Self::UnescapedSlice { slice, .. } => Some(slice),
            _ => None,
        }
    }

    /// Returns `Some(byte)` if this `ByteStringPart` is an `EscapedByte(byte)` and `None`
    /// otherwise.
    pub fn as_escaped(&self) -> Option<u8> {
        match self {
            Self::EscapedByte { byte, .. } => Some(*byte),
            _ => None,
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
