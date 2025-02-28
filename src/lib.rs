pub mod dn;


/// Finds the first occurrence of a character in a string starting at a specific index.
///
/// Slices the string from `start_byte_index` forward, then takes care of adding `start_byte_index`
/// to the returned value.
pub(crate) fn find_next_index(haystack: &str, needle: char, start_byte_index: usize) -> Option<usize> {
    haystack[start_byte_index..].find(needle)
        .map(|i| i + start_byte_index)
}
