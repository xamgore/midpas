use nom::Parser;
use nom_supreme::ParserExt;

use crate::{chr, IResult, keyword};

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
  Not,
  Minus,
}

pub fn not(input: &str) -> IResult<&str, UnOp> {
  keyword("not").value(UnOp::Not).parse(input)
}

pub fn minus(input: &str) -> IResult<&str, UnOp> {
  chr('-').value(UnOp::Minus).parse(input)
}
