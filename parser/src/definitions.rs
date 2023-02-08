use nom::branch::alt;
use nom::multi::many1;
use nom::Parser;
use nom::sequence::pair;
use nom_supreme::multi::collect_separated_terminated;
use nom_supreme::ParserExt;

use crate::{chr, identifier, IResult, keyword};
use crate::functions::{fn_def, FnDef};
use crate::literals::{Lit, literal};
use crate::types::{Type, type_expr, VarDef};
use crate::whitespaces::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Def<'a> {
  Vars(Vec<VarDef<'a>>),
  Consts(Vec<(&'a str, Lit<'a>)>),
  Fn(FnDef<'a>),
  Types(Vec<(&'a str, Type<'a>)>),
}

pub fn definition(input: &str) -> IResult<&str, Def> {
  alt((const_def, var_def, type_def, fn_def.map(Def::Fn)))(input)
}

pub fn const_def(input: &str) -> IResult<&str, Def> {
  keyword("const")
    .precedes(many1(pair(
      identifier.terminated(chr('=')),
      rms0(literal).terminated(chr(';')),
    )))
    .map(Def::Consts)
    .parse(input)
}

pub fn var_def(input: &str) -> IResult<&str, Def> {
  let _ids = collect_separated_terminated(identifier, chr(','), chr(':'));

  keyword("var")
    .precedes(many1(
      // ids: type;
      pair(_ids, type_expr).map(VarDef::from).terminated(chr(';')),
    ))
    .map(Def::Vars)
    .parse(input)
}

pub fn type_def(input: &str) -> IResult<&str, Def> {
  keyword("type")
    .precedes(many1(pair(
      identifier.terminated(chr('=')),
      rms0(type_expr).terminated(chr(';')),
    )))
    .map(Def::Types)
    .parse(input)
}

#[cfg(test)]
mod tests {
  use crate::EntityId;

  use super::*;

  #[test]
  fn parse_const_def() {
    assert_eq!(
      const_def("const X=1; Y=2;").unwrap(),
      ("", Def::Consts(vec![("X", Lit::Int(1)), ("Y", Lit::Int(2))]))
    );
  }

  #[test]
  fn parse_var_def() {
    assert_eq!(
      var_def("var x: integer; y: bool;").unwrap(),
      (
        "",
        Def::Vars(vec![
          VarDef { vars: vec!["x"], of: Type::Named(EntityId::Internal { id: "integer" }) },
          VarDef { vars: vec!["y"], of: Type::Named(EntityId::Internal { id: "bool" }) },
        ])
      )
    );
  }
}
