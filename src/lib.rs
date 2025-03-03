pub mod dn;
pub mod filter;


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
