use nom::branch::alt;
use nom::bytes::complete::{tag, take_until};
use nom::character::complete::{char as chr, not_line_ending};
use nom::combinator::value;
use nom::error::ParseError;
use nom::multi::many0_count;
use nom::Parser;
use nom::sequence::{delimited, pair};

fn line_comment<'a, E: ParseError<&'a str>>(input: &'a str) -> nom::IResult<&str, (), E> {
  value((), pair(tag("//"), not_line_ending))(input)
}

fn multi_line_comment<'a, E: ParseError<&'a str>>(input: &'a str) -> nom::IResult<&'a str, (), E> {
  value(
    (),
    alt((
      delimited(tag("/*"), take_until("*/"), tag("*/")),
      delimited(tag("(*"), take_until("*)"), tag("*)")),
      delimited(chr('{'), take_until("}"), chr('}')),
    )),
  )(input)
}

fn multispace0<'a, E: ParseError<&'a str>>(input: &'a str) -> nom::IResult<&'a str, (), E> {
  value(
    (),
    many0_count(alt((
      line_comment,
      multi_line_comment,
      nom::character::complete::multispace1.map(|_| ()),
    ))),
  )(input)
}

pub fn rms0<'a, O, E: ParseError<&'a str>, P>(
  mut parser: P,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O, E>
  where
    P: Parser<&'a str, O, E>,
{
  move |input: &str| {
    let (input, o1) = parser.parse(input)?;
    multispace0(input).map(|(i, _)| (i, o1))
  }
}

#[deprecated]
pub fn lms0<'a, O, E: ParseError<&'a str>, P>(
  mut parser: P,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O, E>
  where
    P: Parser<&'a str, O, E>,
{
  move |input: &str| {
    let (input, _) = multispace0(input)?;
    parser.parse(input)
  }
}

#[deprecated]
pub fn ms0<'a, O, E: ParseError<&'a str>, P>(
  parser: P,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O, E>
  where
    P: Parser<&'a str, O, E>,
{
  rms0(lms0(parser))
}

#[cfg(test)]
mod tests {
  use nom::error::Error;

  use super::*;

  #[test]
  fn parse_line_comment() {
    assert_eq!(line_comment::<Error<&str>>("// test\n me").unwrap(), ("\n me", ()));
  }

  #[test]
  fn parse_multi_line_comment() {
    assert_eq!(multi_line_comment::<Error<&str>>("/*\n  hello\n */\n").unwrap(), ("\n", ()));
  }

  #[test]
  fn consume_comments_with_whitespaces() {
    let input = "    /*comment*/    ";
    assert_eq!(multispace0::<Error<&str>>(input).unwrap(), ("", ()));
  }

  #[test]
  fn remove_right_spaces() {
    let input = "t1   /*comment*/   t2";
    let res: nom::IResult<&str, (&str, &str), Error<&str>> =
      pair(rms0(tag("t1")), tag("t2"))(input);
    assert_eq!(res.unwrap(), ("", (("t1", "t2"))));
  }
}
