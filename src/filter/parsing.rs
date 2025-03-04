//! Functionality related to parsing LDAP filters.
//!
//! The string representation of LDAP filters is defined in RFC4515.


use std::collections::BTreeSet;

use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{char, satisfy};
use nom::combinator::opt;
use nom::multi::{many0, many1};
use nom::sequence::delimited;
#[cfg(feature = "tracing")] use tracing::{instrument, trace};

use crate::{AttributeType, hex_to_nibble};
use crate::common_parsing::{parse_attribute_key, PResult};
use crate::filter::{AssertionValue, AttributeDescription, Filter, LdapOption};
#[cfg(not(feature = "tracing"))] use crate::no_trace as trace;


#[cfg_attr(feature = "tracing", instrument(skip_all))]
pub(crate) fn parse_attribute_description(rest: &str) -> PResult<AttributeDescription> {
    trace!("rest is {:?}", rest);
    let (rest, attribute_type) = parse_attribute_key(rest)?;
    let (rest, options_vec) = many0(parse_attribute_option)
        .parse(rest)?;
    let options: BTreeSet<LdapOption> = options_vec
        .into_iter()
        .collect();
    let descr = AttributeDescription {
        attribute_type,
        options,
    };
    Ok((rest, descr))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_attribute_option(rest: &str) -> PResult<LdapOption> {
    trace!("rest is {:?}", rest);
    let (rest, _) = char(';')(rest)?;
    let (rest, option) = take_while(|c| c == '-' || (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
        .parse(rest)?;
    let o = LdapOption {
        name: option.to_owned(),
    };
    Ok((rest, o))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
pub(crate) fn parse_filter(rest: &str) -> PResult<Filter> {
    trace!("rest is {:?}", rest);
    delimited(
        char('('),
        parse_filter_content,
        char(')')
    )
        .parse(rest)
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_filter_content(rest: &str) -> PResult<Filter> {
    trace!("rest is {:?}", rest);
    alt((
        parse_and_filter_content,
        parse_or_filter_content,
        parse_not_filter_content,
        parse_item_filter_content,
    ))
        .parse(rest)
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_and_filter_content(rest: &str) -> PResult<Filter> {
    trace!("rest is {:?}", rest);
    let (rest, _) = char('&')(rest)?;
    let (rest, criteria_vec) = parse_filter_list(rest)?;
    let criteria: BTreeSet<Filter> = criteria_vec
        .into_iter()
        .collect();
    Ok((rest, Filter::And(criteria)))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_or_filter_content(rest: &str) -> PResult<Filter> {
    trace!("rest is {:?}", rest);
    let (rest, _) = char('|')(rest)?;
    let (rest, criteria_vec) = parse_filter_list(rest)?;
    let criteria: BTreeSet<Filter> = criteria_vec
        .into_iter()
        .collect();
    Ok((rest, Filter::Or(criteria)))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_filter_list(rest: &str) -> PResult<Vec<Filter>> {
    trace!("rest is {:?}", rest);
    many1(parse_filter)
        .parse(rest)
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_not_filter_content(rest: &str) -> PResult<Filter> {
    trace!("rest is {:?}", rest);
    let (rest, _) = char('!')(rest)?;
    let (rest, criterion) = parse_filter(rest)?;
    Ok((rest, Filter::Not(Box::new(criterion))))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_item_filter_content(rest: &str) -> PResult<Filter> {
    trace!("rest is {:?}", rest);
    // simple / present / substring / extensible
    // parse_substring_filter_content handles "present" as a special case
    alt((
        parse_substring_filter_content,
        parse_simple_filter_content,
        parse_extensible_filter_content,
    ))
        .parse(rest)
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_simple_filter_content(rest: &str) -> PResult<Filter> {
    trace!("rest is {:?}", rest);
    let (rest, key) = parse_attribute_description(rest)?;
    let (rest, operator) = alt((
        tag("="),
        tag("~="),
        tag(">="),
        tag("<="),
    ))
        .parse(rest)?;
    let (rest, value) = parse_assertion_value(rest)?;
    let filter = match operator {
        "=" => Filter::Equality { key, value },
        "~=" => Filter::Approximate { key, value },
        ">=" => Filter::GreaterOrEqual { key, value },
        "<=" => Filter::LessOrEqual { key, value },
        _ => unreachable!(),
    };
    Ok((rest, filter))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
pub(crate) fn parse_assertion_value(rest: &str) -> PResult<AssertionValue> {
    trace!("rest is {:?}", rest);
    let (rest, byte_chunks) = many0(parse_assertion_value_char)
        .parse(rest)?;

    let mut bytes = Vec::with_capacity(byte_chunks.iter().map(|v| v.len()).sum());
    for chunk in byte_chunks {
        bytes.extend(&chunk);
    }

    let value = AssertionValue {
        bytes,
    };
    Ok((rest, value))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_assertion_value_char(rest: &str) -> PResult<Vec<u8>> {
    trace!("rest is {:?}", rest);
    alt((
        parse_assertion_value_normal_char,
        parse_assertion_value_escaped_char,
    ))
        .parse(rest)
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_assertion_value_normal_char(rest: &str) -> PResult<Vec<u8>> {
    trace!("rest is {:?}", rest);
    let (rest, character) = satisfy(|c|
        c != '\u{00}' && c != '(' && c != ')' && c != '*' && c != '\\'
    )
        .parse(rest)?;
    let mut buf = [0u8; 4];
    let vector = character.encode_utf8(&mut buf).as_bytes().to_vec();
    Ok((rest, vector))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_assertion_value_escaped_char(rest: &str) -> PResult<Vec<u8>> {
    trace!("rest is {:?}", rest);
    let (rest, _) = tag("\\")(rest)?;
    let (rest, t) = satisfy(|c| c.is_ascii_hexdigit())(rest)?;
    let (rest, b) = satisfy(|c| c.is_ascii_hexdigit())(rest)?;
    let top_nibble = hex_to_nibble(t);
    assert_eq!(top_nibble & 0xF0, 0);
    let bottom_nibble = hex_to_nibble(b);
    assert_eq!(bottom_nibble & 0xF0, 0);
    let byte = (top_nibble << 4) | bottom_nibble;
    Ok((rest, vec![byte]))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_substring_filter_content(rest: &str) -> PResult<Filter> {
    trace!("rest is {:?}", rest);
    let (rest, key) = parse_attribute_description(rest)?;
    let (rest, _) = tag("=")(rest)?;
    let (rest, start) = parse_assertion_value(rest)?;
    let (rest, middle) = parse_substring_filter_anys(rest)?;
    let (rest, end) = parse_assertion_value(rest)?;

    // special-case handling the "present" filter, since the above statements match it as well
    if start.as_bytes().len() == 0 && middle.len() == 0 && end.as_bytes().len() == 0 {
        return Ok((rest, Filter::Present { key }));
    }

    Ok((rest, Filter::Substring {
        key,
        start: start.into_option(),
        middle,
        end: end.into_option(),
    }))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_substring_filter_anys(rest: &str) -> PResult<Vec<AssertionValue>> {
    trace!("rest is {:?}", rest);

    // grab an asterisk
    let (rest, _) = tag("*")(rest)?;

    // grab additional values followed by asterisks
    let (rest, parts) = many0(parse_substring_filter_any_part)
        .parse(rest)?;

    Ok((rest, parts))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_substring_filter_any_part(rest: &str) -> PResult<AssertionValue> {
    trace!("rest is {:?}", rest);

    // grab a value
    let (rest, value) = parse_assertion_value(rest)?;

    // grab an asterisk
    let (rest, _) = tag("*")(rest)?;

    Ok((rest, value))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_extensible_filter_content(rest: &str) -> PResult<Filter> {
    trace!("rest is {:?}", rest);

    alt((
        parse_extensible_filter_content_attr,
        parse_extensible_filter_content_no_attr,
    ))
        .parse(rest)
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_extensible_filter_content_attr(rest: &str) -> PResult<Filter> {
    eprintln!("PEFCA/START: rest is {:?}", rest);
    trace!("rest is {:?}", rest);

    // attr [dnattrs] [matchingrule] ":=" assertionvalue
    let (rest, key) = parse_attribute_description(rest)?;
    eprintln!("PEFCA/AD: rest is {:?}", rest);
    let (rest, dn_attributes) = opt(parse_dnattrs)
        .parse(rest)?;
    eprintln!("PEFCA/DNA: rest is {:?}", rest);
    let (rest, matching_rule) = opt(parse_matching_rule)
        .parse(rest)?;
    eprintln!("PEFCA/MR: rest is {:?}", rest);
    let (rest, _) = tag(":=")(rest)?;
    eprintln!("PEFCA/SYM: rest is {:?}", rest);
    let (rest, value) = parse_assertion_value(rest)?;
    eprintln!("PEFCA/AV: rest is {:?}", rest);
    let filter = Filter::Extensible {
        matching_rule,
        key: Some(key),
        value,
        dn_attributes: dn_attributes.is_some(),
    };
    Ok((rest, filter))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_extensible_filter_content_no_attr(rest: &str) -> PResult<Filter> {
    trace!("rest is {:?}", rest);

    // [dnattrs] matchingrule ":=" assertionvalue
    let (rest, dn_attributes) = opt(parse_dnattrs)
        .parse(rest)?;
    eprintln!("PEFCnA/DNA: rest is {:?}", rest);
    let (rest, matching_rule) = parse_matching_rule(rest)?;
    eprintln!("PEFCnA/MR: rest is {:?}", rest);
    let (rest, _) = tag(":=")(rest)?;
    eprintln!("PEFCnA/SYM: rest is {:?}", rest);
    let (rest, value) = parse_assertion_value(rest)?;
    eprintln!("PEFCnA/AV: rest is {:?}", rest);
    let filter = Filter::Extensible {
        matching_rule: Some(matching_rule),
        key: None,
        value,
        dn_attributes: dn_attributes.is_some(),
    };
    Ok((rest, filter))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_dnattrs(rest: &str) -> PResult<()> {
    trace!("rest is {:?}", rest);

    let (rest, _) = tag(":dn")(rest)?;
    Ok((rest, ()))
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_matching_rule(rest: &str) -> PResult<AttributeType> {
    trace!("rest is {:?}", rest);

    let (rest, _) = tag(":")(rest)?;
    parse_attribute_key(rest)
}
