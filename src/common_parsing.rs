use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{char, satisfy};
use nom::combinator::recognize;
use nom::multi::separated_list1;
use nom::sequence::pair;
#[cfg(feature = "tracing")] use tracing::{instrument, trace};

use crate::{AttributeType, OidAttributeType, ShortAttributeType};
#[cfg(not(feature = "tracing"))] use crate::no_trace as trace;


/// The result type of combinable parsers in this crate.
pub(crate) type PResult<'a, O> = nom::IResult<&'a str, O>;


#[cfg_attr(feature = "tracing", instrument(skip_all))]
pub(crate) fn parse_attribute_key(rest: &str) -> PResult<AttributeType> {
    trace!("rest is {:?}", rest);
    alt((
        |r| parse_attribute_key_oid(r).map(|(rr, oid)| (rr, AttributeType::Oid(oid))),
        |r| parse_attribute_key_short(r).map(|(rr, short)| (rr, AttributeType::Short(short))),
    ))
        .parse(rest)
}

#[cfg_attr(feature = "tracing", instrument(skip_all))]
pub(crate) fn parse_attribute_key_oid(rest: &str) -> PResult<OidAttributeType> {
    trace!("rest is {:?}", rest);
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

#[cfg_attr(feature = "tracing", instrument(skip_all))]
fn parse_oid_arc(rest: &str) -> PResult<&str> {
    trace!("rest is {:?}", rest);
    #[cfg_attr(feature = "tracing", instrument(skip_all))]
    fn parse_zero(rest: &str) -> PResult<&str> {
        trace!("rest is {:?}", rest);
        tag("0")(rest)
    }
    #[cfg_attr(feature = "tracing", instrument(skip_all))]
    fn parse_decimal_without_leading_zeroes(rest: &str) -> PResult<&str> {
        trace!("rest is {:?}", rest);
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

#[cfg_attr(feature = "tracing", instrument(skip_all))]
pub(crate) fn parse_attribute_key_short(rest: &str) -> PResult<ShortAttributeType> {
    trace!("rest is {:?}", rest);
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
