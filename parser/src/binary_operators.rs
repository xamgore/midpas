use nom::branch::alt;
use nom::combinator::value;

use crate::{chr, IResult, keyword, tag};
use crate::whitespaces::*;

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
  Less,
  LessEq,
  Greater,
  GreaterEq,
  Equal,
  NotEqual,
  Plus,
  Minus,
  And,
  Or,
  Xor,
  Mul,
  Div,
  UShiftRight,
  ShiftRight,
  ShiftLeft,
  IntDiv,
  IntMod,
}

pub fn relative_operator(input: &str) -> IResult<&str, BinOp> {
  rms0(alt((
    value(BinOp::LessEq, tag("<=")),
    value(BinOp::GreaterEq, tag(">=")),
    value(BinOp::NotEqual, tag("<>")),
    value(BinOp::Less, chr('<')),
    value(BinOp::Greater, chr('>')),
    value(BinOp::Equal, chr('=')),
  )))(input)
}

pub fn sum_operator(input: &str) -> IResult<&str, BinOp> {
  alt((
    value(BinOp::Plus, chr('+')),
    value(BinOp::Minus, chr('-')),
    value(BinOp::Or, keyword("or")),
    value(BinOp::Xor, keyword("xor")),
  ))(input)
}

pub fn mul_operator(input: &str) -> IResult<&str, BinOp> {
  alt((
    value(BinOp::Mul, chr('*')),
    value(BinOp::Div, chr('/')),
    value(BinOp::IntDiv, keyword("div")),
    value(BinOp::IntMod, keyword("mod")),
    value(BinOp::And, keyword("and")),
    value(BinOp::UShiftRight, keyword("ushr")),
    value(BinOp::ShiftRight, keyword("shr")),
    value(BinOp::ShiftLeft, keyword("shl")),
  ))(input)
}
