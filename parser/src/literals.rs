use nom::branch::alt;
use nom::bytes::complete::take_until;
use nom::character::complete::{alphanumeric1, digit1, i64 as parse_i64, u32 as parse_u32};
use nom::combinator::{map, map_opt, map_res, value};
use nom::Parser;
use nom::sequence::{delimited, preceded};
use nom_supreme::ParserExt;

use crate::{chr, IResult, keyword};
use crate::whitespaces::rms0;

#[derive(Debug, Clone, PartialEq)]
pub enum Lit<'a> {
  IntHex(i64),
  Int(i64),
  Real(f32),
  Bool(bool),
  Char(char),
  CharHex(char),
  Str(&'a str),
}

pub fn literal(input: &str) -> IResult<&str, Lit> {
  alt((bool_lit, real_lit, int_lit, char_lit, string_lit))(input)
}

pub fn bool_lit(input: &str) -> IResult<&str, Lit> {
  alt((value(Lit::Bool(true), keyword("true")), value(Lit::Bool(false), keyword("false"))))(input)
}

pub fn int_lit(input: &str) -> IResult<&str, Lit> {
  let from_hex = |out: &str| i64::from_str_radix(out, 16).map(Lit::IntHex);
  alt((map_res(preceded(chr('$'), alphanumeric1), from_hex), map(parse_i64, Lit::Int)))(input)
}

pub fn real_lit(input: &str) -> IResult<&str, Lit> {
  chr('.')
    .delimited_by(digit1)
    .recognize()
    .map_res(|val| std::str::FromStr::from_str(val).map(Lit::Real))
    .parse(input)
}

pub fn char_lit(input: &str) -> IResult<&str, Lit> {
  let parse_hex = chr('$').precedes(alphanumeric1.map_res(|out| u32::from_str_radix(out, 16)));

  rms0(preceded(
    chr('#'),
    alt((
      map_opt(parse_u32, |val| char::from_u32(val).map(Lit::Char)),
      map_opt(parse_hex, |val| char::from_u32(val).map(Lit::CharHex)),
    )),
  ))(input)
}

// todo: escaping && double quotes?
pub fn string_lit(input: &str) -> IResult<&str, Lit> {
  map(
    alt((
      delimited(chr('\''), take_until("\'"), chr('\'')),
      delimited(chr('\"'), take_until("\""), chr('\"')),
    )),
    Lit::Str,
  )(input)
}

#[cfg(test)]
mod tests {
  use super::*;

// todo: chat_lit

  #[test]
  fn parse_hex_integer() {
    assert_eq!(int_lit("$A0").unwrap(), ("", Lit::IntHex(160)));
  }

  #[test]
  fn parse_bool_lit() {
    assert_eq!(bool_lit("true").unwrap(), ("", Lit::Bool(true)));
    assert_eq!(bool_lit("false").unwrap(), ("", Lit::Bool(false)));
  }

  #[test]
  fn parse_int_lit() {
    assert_eq!(int_lit("42").unwrap(), ("", Lit::Int(42)));
    assert_eq!(int_lit("-42").unwrap(), ("", Lit::Int(-42)));
  }

  #[test]
  fn parse_real_lit() {
    assert_eq!(real_lit("42.42").unwrap(), ("", Lit::Real(42.42)));
    assert_eq!(real_lit("0.5").unwrap(), ("", Lit::Real(0.5)));
    assert_eq!(real_lit("0.25").unwrap(), ("", Lit::Real(0.25)));
    assert_eq!(real_lit("1.0").unwrap(), ("", Lit::Real(1.0)));
    assert_eq!(real_lit("2.0").unwrap(), ("", Lit::Real(2.0)));
  }

  #[test]
  fn parse_string_lit() {
    assert_eq!(string_lit(r#""xxx""#).unwrap(), ("", Lit::Str("xxx")));
    assert_eq!(string_lit(r#"'x'"#).unwrap(), ("", Lit::Str("x")));
  }
}
