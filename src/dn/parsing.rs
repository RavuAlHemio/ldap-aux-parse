use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{char, satisfy};
use nom::combinator::recognize;
use nom::error::ErrorKind;
use nom::multi::{many1, separated_list0, separated_list1};
use nom::sequence::pair;

use crate::dn::{
    AttributeType, AttributeValue, DistinguishedName, OidAttributeType, RelativeDistinguishedName,
    ShortAttributeType,
};


type PResult<'a, O> = nom::IResult<&'a str, O>;


pub(crate) fn parse_dn(rest: &str) -> PResult<DistinguishedName> {
    let (rest, rdns) = separated_list0(
        char(','),
        parse_rdn,
    )
        .parse(rest)?;
    let dn = DistinguishedName::from(rdns);
    Ok((rest, dn))
}

pub(crate) fn parse_rdn(rest: &str) -> PResult<RelativeDistinguishedName> {
    let (rest, kvps) = separated_list0(
        char('+'),
        parse_rdn_kvp,
    )
        .parse(rest)?;
    let rdn = RelativeDistinguishedName { 
        key_value_pairs: kvps.into_iter().collect(),
    };
    Ok((rest, rdn))
}

fn parse_rdn_kvp(rest: &str) -> PResult<(AttributeType, AttributeValue)> {
    let (rest, key) = parse_rdn_key(rest)?;
    let (rest, _) = char('=')(rest)?;
    let (rest, value) = parse_rdn_value(rest)?;
    Ok((rest, (key, value)))
}

pub(crate) fn parse_rdn_key(rest: &str) -> PResult<AttributeType> {
    alt((
        |r| parse_rdn_key_oid(r).map(|(rr, oid)| (rr, AttributeType::Oid(oid))),
        |r| parse_rdn_key_short(r).map(|(rr, short)| (rr, AttributeType::Short(short))),
    ))
        .parse(rest)
}

pub(crate) fn parse_rdn_key_oid(rest: &str) -> PResult<OidAttributeType> {
    let (rest, arcs) = separated_list1(
        char('.'),
        parse_oid_arc,
    )
        .parse(rest)?;
    let owned_arcs = arcs.into_iter()
        .map(|s| s.to_owned())
        .collect();
    let oid = OidAttributeType {
        arcs: owned_arcs,
    };
    Ok((rest, oid))
}

fn parse_oid_arc(rest: &str) -> PResult<&str> {
    fn parse_zero(rest: &str) -> PResult<&str> {
        tag("0")(rest)
    }
    fn parse_decimal_without_leading_zeroes(rest: &str) -> PResult<&str> {
        recognize(pair(
            satisfy(|c| c >= '1' && c <= '9'),
            take_while(|c| c >= '0' && c <= '9'),
        ))
            .parse(rest)
    }
    alt((
        parse_zero,
        parse_decimal_without_leading_zeroes,
    ))
        .parse(rest)
}

pub(crate) fn parse_rdn_key_short(rest: &str) -> PResult<ShortAttributeType> {
    let (rest, key) = recognize(pair(
        satisfy(|c| (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')),
        take_while(|c| c == '-' || (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')),
    ))
        .parse(rest)?;
    let attribute_type = ShortAttributeType {
        name: key.to_owned(),
    };
    Ok((rest, attribute_type))
}

pub(crate) fn parse_rdn_value(rest: &str) -> PResult<AttributeValue> {
    alt((
        parse_rdn_value_hex,
        parse_rdn_value_text,
    ))
        .parse(rest)
}

fn hex_to_nibble(hex: char) -> u8 {
    match hex {
        '0'..='9' => ((hex as u32) - ('0' as u32)) as u8,
        'A'..='F' => ((hex as u32) + 10 - ('A' as u32)) as u8,
        'a'..='f' => ((hex as u32) + 10 - ('a' as u32)) as u8,
        _ => unreachable!(),
    }
}

pub(crate) fn parse_rdn_value_hex(rest: &str) -> PResult<AttributeValue> {
    let (rest, _) = tag("#")(rest)?;
    let (rest, hex_pairs) = many1(pair(
        satisfy(|c| c.is_ascii_hexdigit()),
        satisfy(|c| c.is_ascii_hexdigit()),
    ))
        .parse(rest)?;
    let mut bytes = Vec::with_capacity(hex_pairs.len() * 2);
    for (t, b) in hex_pairs {
        let top_nibble = hex_to_nibble(t);
        assert_eq!(top_nibble & 0xF0, 0);
        let bottom_nibble = hex_to_nibble(b);
        assert_eq!(bottom_nibble & 0xF0, 0);
        let byte = (top_nibble << 4) | bottom_nibble;
        bytes.push(byte);
    }
    let value = AttributeValue {
        bytes,
    };
    Ok((rest, value))
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum PotentialEscape {
    Char(char),
    EscapedByte(u8),
}

trait StrictIdentifierParser<T> {
    /// Whether empty identifiers are to be allowed as well.
    fn allow_empty() -> bool;

    /// Grabs an input token from the given string slice. On success, returns the number of bytes
    /// occupied by the token as well as the token's value.
    fn grab_token(s: &str) -> Result<(usize, T), nom::Err<nom::error::Error<&str>>>;

    /// Predicate returning whether the token is generally valid for this identifier in at least the
    /// medial position.
    fn predicate(token: &T) -> bool;

    /// Predicate returning whether the generally-valid token is also valid in the initial position.
    fn additional_start_predicate(token: &T) -> bool;

    /// Predicate returning whether the generally-valid token is also valid in the final position.
    fn additional_end_predicate(token: &T) -> bool;

    /// Appends the contents of the token to the given byte vector.
    fn append_token(token: &T, target: &mut Vec<u8>);

    fn parse(input: &str) -> PResult<Vec<u8>> {
        let mut ret = Vec::new();
        let mut rest_start = 0;
        let mut best_split_state = if Self::allow_empty() {
            Some((0, ret.clone()))
        } else {
            None
        };

        fn get_best_state(input: &str, best_split_state: Option<(usize, Vec<u8>)>) -> PResult<Vec<u8>> {
            if let Some((rest_start, ret)) = best_split_state {
                Ok((&input[rest_start..], ret))
            } else {
                // nope, we never parsed a valid ending character
                Err(nom::Err::Error(nom::error::Error::new(input, ErrorKind::Satisfy)))
            }
        }

        // take at least one token (unless we're okay with empty)
        let (first_token_length, first_token) = match Self::grab_token(&input[rest_start..]) {
            Ok(ftl_ft) => ftl_ft,
            Err(nom::Err::Error(_)) => {
                // we ran out of valid tokens
                // return what we have, if anything
                return get_best_state(input, best_split_state);
            },
            Err(e) => return Err(e),
        };
        // it must fulfil our general predicate
        if !Self::predicate(&first_token) {
            return get_best_state(input, best_split_state);
        }

        // since it is the first, it has to fulfil the first-token predicate too
        if !Self::additional_start_predicate(&first_token) {
            return get_best_state(input, best_split_state);
        }

        // good; remember it
        Self::append_token(&first_token, &mut ret);
        rest_start += first_token_length;

        // if it also passes our ending predicate, remember it as hitherto the best ending point
        if Self::additional_end_predicate(&first_token) {
            best_split_state = Some((rest_start, ret.clone()));
        }

        // try the next one
        loop {
            let (next_token_length, next_token) = match Self::grab_token(&input[rest_start..]) {
                Ok(ntl_nt) => ntl_nt,
                Err(nom::Err::Error(_)) => {
                    // we ran out of valid tokens
                    // return what we have, if anything
                    return get_best_state(input, best_split_state);
                },
                Err(e) => return Err(e),
            };

            // again, is this a valid token for our needs?
            if !Self::predicate(&next_token) {
                // no; return what we have
                return get_best_state(input, best_split_state);
            }

            Self::append_token(&next_token, &mut ret);
            rest_start += next_token_length;

            // is this a valid end token?
            if Self::additional_end_predicate(&next_token) {
                // remember it for when we fail
                best_split_state = Some((rest_start, ret.clone()));
            }
        }
    }
}
struct DnIdentifierParser;
impl StrictIdentifierParser<PotentialEscape> for DnIdentifierParser {
    fn allow_empty() -> bool { true }

    fn grab_token(rest: &str) -> Result<(usize, PotentialEscape), nom::Err<nom::error::Error<&str>>> {
        let mut rest_chars = rest.chars();
        let first_char = match rest_chars.next() {
            Some(c) => c,
            None => return Err(nom::Err::Error(nom::error::Error::new(rest, ErrorKind::Satisfy))),
        };
        if first_char != '\\' {
            return Ok((first_char.len_utf8(), PotentialEscape::Char(first_char)));
        }

        let escaped_char = match rest_chars.next() {
            Some(c) => c,
            None => return Err(nom::Err::Error(nom::error::Error::new(rest, ErrorKind::Satisfy))),
        };
        match escaped_char {
            '"'|'+'|','|';'|'<'|'>'|' '|'#'|'=' => {
                // self-escape
                assert_eq!(escaped_char.len_utf8(), 1);
                let rest_point = first_char.len_utf8() + escaped_char.len_utf8();
                Ok((rest_point, PotentialEscape::EscapedByte((escaped_char as u32) as u8)))
            },
            '0'..='9'|'A'..='F'|'a'..='f' => {
                // hex escape
                let other_escaped_char = match rest_chars.next() {
                    Some(c) => c,
                    None => return Err(nom::Err::Error(nom::error::Error::new(rest, ErrorKind::Satisfy))),
                };
                match other_escaped_char {
                    '0'..='9'|'A'..='F'|'a'..='f' => {
                        let top_nibble = hex_to_nibble(escaped_char);
                        assert_eq!(top_nibble & 0xF0, 0);
                        let bottom_nibble = hex_to_nibble(escaped_char);
                        assert_eq!(bottom_nibble & 0xF0, 0);
                        let byte = (top_nibble << 4) | bottom_nibble;
                        let rest_point = first_char.len_utf8() + escaped_char.len_utf8() + other_escaped_char.len_utf8();
                        Ok((rest_point, PotentialEscape::EscapedByte(byte)))
                    },
                    _ => Err(nom::Err::Error(nom::error::Error::new(rest, ErrorKind::Satisfy))),
                }
            },
            _ => Err(nom::Err::Error(nom::error::Error::new(rest, ErrorKind::Satisfy))),
        }
    }

    fn predicate(token: &PotentialEscape) -> bool {
        match *token {
            PotentialEscape::Char(c) => {
                c != '\u{00}' && c != '"' // allow ' ' and '#'
                    && c != '+' && c != ',' && c != ';' && c != '<'
                    && c != '>' && c != '\\'
            },
            PotentialEscape::EscapedByte(_) => true,
        }
    }

    fn additional_start_predicate(token: &PotentialEscape) -> bool {
        match *token {
            PotentialEscape::Char(c) => c != ' ' && c != '#',
            PotentialEscape::EscapedByte(_) => true,
        }
    }

    fn additional_end_predicate(token: &PotentialEscape) -> bool {
        match *token {
            PotentialEscape::Char(c) => c != ' ',
            PotentialEscape::EscapedByte(_) => true,
        }
    }

    fn append_token(token: &PotentialEscape, target: &mut Vec<u8>) {
        match token {
            PotentialEscape::Char(c) => {
                let mut buf = [0u8; 4];
                let encoded = c.encode_utf8(&mut buf);
                target.extend_from_slice(encoded.as_bytes());
            },
            PotentialEscape::EscapedByte(b) => target.push(*b),
        }
    }
}

pub(crate) fn parse_rdn_value_text(rest: &str) -> PResult<AttributeValue> {
    DnIdentifierParser::parse(rest)
        .map(|(rest, bytes)| (rest, AttributeValue { bytes }))
}


#[cfg(test)]
mod tests {
    use super::{AttributeValue, DistinguishedName};

    #[test]
    fn test_parse_attribute_value() {
        let value = AttributeValue::try_from_str("").unwrap();
        assert_eq!(value.bytes, b"");

        let value = AttributeValue::try_from_str("abc").unwrap();
        assert_eq!(value.bytes, b"abc");

        let value = AttributeValue::try_from_str("#616263").unwrap();
        assert_eq!(value.bytes, b"abc");

        let value = AttributeValue::try_from_str("Dewey\\, Cheatham & Howe").unwrap();
        assert_eq!(value.bytes, b"Dewey, Cheatham & Howe");

        let value = AttributeValue::try_from_str("\\ leading space").unwrap();
        assert_eq!(value.bytes, b" leading space");

        let value = AttributeValue::try_from_str("\\   leading spaces").unwrap();
        assert_eq!(value.bytes, b"   leading spaces");

        let value = AttributeValue::try_from_str("trailing space\\ ").unwrap();
        assert_eq!(value.bytes, b"trailing space ");

        let value = AttributeValue::try_from_str("trailing spaces  \\ ").unwrap();
        assert_eq!(value.bytes, b"trailing spaces   ");

        let value = AttributeValue::try_from_str("\\#").unwrap();
        assert_eq!(value.bytes, b"#");

        let value = AttributeValue::try_from_str("#23").unwrap();
        assert_eq!(value.bytes, b"#");

        assert_eq!(AttributeValue::try_from_str("Dewey, Cheatham & Howe"), None);
        assert_eq!(AttributeValue::try_from_str(" leading space"), None);
        assert_eq!(AttributeValue::try_from_str("trailing space "), None);
        assert_eq!(AttributeValue::try_from_str("#61\\36\\3263"), None);
    }

    #[test]
    fn test_parse_dn() {
        let value = DistinguishedName::try_from_str("dc=example,dc=com").unwrap();
        assert_eq!(value.as_rdns().len(), 2);
        assert_eq!(value.as_rdns()[0].as_set().len(), 1);
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_short().unwrap(), "dc");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().1.as_bytes(), b"example");
        assert_eq!(value.as_rdns()[1].as_set().len(), 1);
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().0.as_short().unwrap(), "dc");
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().1.as_bytes(), b"com");

        let value = DistinguishedName::try_from_str("o=Dewey\\, Cheatham & Howe,l=London,c=GB").unwrap();
        assert_eq!(value.as_rdns().len(), 3);
        assert_eq!(value.as_rdns()[0].as_set().len(), 1);
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_short().unwrap(), "o");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().1.as_bytes(), b"Dewey, Cheatham & Howe");
        assert_eq!(value.as_rdns()[1].as_set().len(), 1);
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().0.as_short().unwrap(), "l");
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().1.as_bytes(), b"London");
        assert_eq!(value.as_rdns()[2].as_set().len(), 1);
        assert_eq!(value.as_rdns()[2].as_set().first().unwrap().0.as_short().unwrap(), "c");
        assert_eq!(value.as_rdns()[2].as_set().first().unwrap().1.as_bytes(), b"GB");

        let value = DistinguishedName::try_from_str("2.5.4.10=Dewey\\, Cheatham & Howe,l=London,c=GB").unwrap();
        assert_eq!(value.as_rdns().len(), 3);
        assert_eq!(value.as_rdns()[0].as_set().len(), 1);
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_oid().unwrap().len(), 4);
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_oid().unwrap()[0], "2");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_oid().unwrap()[1], "5");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_oid().unwrap()[2], "4");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().0.as_oid().unwrap()[3], "10");
        assert_eq!(value.as_rdns()[0].as_set().first().unwrap().1.as_bytes(), b"Dewey, Cheatham & Howe");
        assert_eq!(value.as_rdns()[1].as_set().len(), 1);
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().0.as_short().unwrap(), "l");
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().1.as_bytes(), b"London");
        assert_eq!(value.as_rdns()[2].as_set().len(), 1);
        assert_eq!(value.as_rdns()[2].as_set().first().unwrap().0.as_short().unwrap(), "c");
        assert_eq!(value.as_rdns()[2].as_set().first().unwrap().1.as_bytes(), b"GB");

        let value = DistinguishedName::try_from_str("l=Biel+l=Bienne,c=CH").unwrap();
        assert_eq!(value.as_rdns().len(), 2);
        assert_eq!(value.as_rdns()[0].as_set().len(), 2);
        assert_eq!(value.as_rdns()[0].as_set().iter().nth(0).unwrap().0.as_short().unwrap(), "l");
        assert_eq!(value.as_rdns()[0].as_set().iter().nth(0).unwrap().1.as_bytes(), b"Biel");
        assert_eq!(value.as_rdns()[0].as_set().iter().nth(1).unwrap().0.as_short().unwrap(), "l");
        assert_eq!(value.as_rdns()[0].as_set().iter().nth(1).unwrap().1.as_bytes(), b"Bienne");
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().0.as_short().unwrap(), "c");
        assert_eq!(value.as_rdns()[1].as_set().first().unwrap().1.as_bytes(), b"CH");
    }
}
