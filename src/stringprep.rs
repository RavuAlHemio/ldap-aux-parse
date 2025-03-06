//! String preparation for comparison according to RFC4518.


use std::borrow::Cow;


// we accept &str, which means the Transcode step is left to the application


/// Performs the Map step of string preparation.
fn map(s: &str) -> Cow<str> {
    fn map_to_nothing(c: char) -> bool {
        // first paragraph
        c == '\u{AD}' || c == '\u{1806}' || c == '\u{34F}'
        || (c >= '\u{180B}' && c <= '\u{180D}')
        || (c >= '\u{FE00}' && c <= '\u{FE0F}')
        || c == '\u{FFFC}'

        // third paragraph
        || (c >= '\u{0}' && c <= '\u{8}')
        || (c >= '\u{E}' && c <= '\u{1F}')
        || (c >= '\u{7F}' && c <= '\u{84}')
        || (c >= '\u{86}' && c <= '\u{9F}')
        || c == '\u{6DD}' || c == '\u{70F}' || c == '\u{180E}'
        || (c >= '\u{200C}' && c <= '\u{200F}')
        || (c >= '\u{202A}' && c <= '\u{202E}')
        || (c >= '\u{2060}' && c <= '\u{2063}')
        || (c >= '\u{206A}' && c <= '\u{206F}')
        || c == '\u{FEFF}'
        || (c >= '\u{FFF9}' && c <= '\u{FFFB}')
        || (c >= '\u{1D173}' && c <= '\u{1D17A}')
        || c == '\u{E0001}'
        || (c >= '\u{E0020}' && c <= '\u{E007F}')

        // fourth paragraph
        || c == '\u{200B}'
    }

    fn map_to_space(c: char) -> bool {
        // second paragraph
        c == '\u{9}' || c == '\u{A}' || c == '\u{B}' || c == '\u{C}'
        || c == '\u{D}' || c == '\u{85}'

        // fourth paragraph
        || c == '\u{20}' || c == '\u{A0}' || c == '\u{1680}'
        || (c >= '\u{2000}' && c <= '\u{200A}')
        || c == '\u{2028}' || c == '\u{2029}' || c == '\u{202F}'
        || c == '\u{205F}' || c == '\u{3000}'
    }

    todo!();
}
