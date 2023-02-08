use derive_more::From;
use nom::branch::alt;
use nom::combinator::rest;
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, tuple};
use nom::Parser;
use nom_supreme::multi::collect_separated_terminated;
use nom_supreme::ParserExt;

use crate::definitions::{definition, Def};
use crate::statements::{begin_end_stmt, Stmt};
use crate::whitespaces::multispace0;
use crate::{chr, identifier, keyword, IResult};

#[derive(Debug, Clone, From, PartialEq)]
pub enum Module<'a> {
  Program(Program<'a>),
  Unit(Unit<'a>),
}

#[derive(Debug, Clone, From, PartialEq)]
pub struct Program<'a> {
  pub name: &'a str,
  pub impl_sect: Section<'a>,
  pub body: Stmt<'a>,
}

#[derive(Debug, Clone, From, PartialEq)]
pub struct Unit<'a> {
  pub name: &'a str,
  pub intf_section: Section<'a>,
  pub impl_sect: Section<'a>,
  // pub initialization: Stmt<'a>, // todo?
}

#[derive(Debug, Clone, From, PartialEq)]
pub struct Section<'a> {
  pub imports: Vec<&'a str>,
  pub definitions: Vec<Def<'a>>,
}

pub fn module(input: &str) -> IResult<&str, Module> {
  multispace0.precedes(alt((program.map(Module::from), unit.map(Module::from)))).parse(input)
}

pub fn unit(input: &str) -> IResult<&str, Unit> {
  tuple((
    delimited(keyword("unit"), identifier, chr(';')),
    keyword("interface").precedes(imports.and(many0(definition)).map(Section::from)),
    delimited(
      keyword("implementation"),
      imports.and(many0(definition)).map(Section::from),
      keyword("end").terminated(chr('.')).terminated(rest),
    ),
  ))
  .map(Unit::from)
  .parse(input)
}

pub fn program(input: &str) -> IResult<&str, Program> {
  tuple((
    delimited(keyword("program"), identifier, chr(';')),
    imports.and(many0(definition)).map(Section::from),
    // ignore everything after end.
    begin_end_stmt.terminated(chr('.')).terminated(rest),
  ))
  .map(Program::from)
  .parse(input)
}

fn imports(input: &str) -> IResult<&str, Vec<&str>> {
  let uses = keyword("uses").precedes(collect_separated_terminated(identifier, chr(','), chr(';')));

  fold_many0(uses, Vec::new, |mut acc: Vec<_>, items: Vec<_>| {
    acc.extend(items);
    acc
  })
  .parse(input)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_use_section() {
    assert_eq!(imports("uses p1, p2;").unwrap(), ("", (vec!["p1", "p2"])));
  }

  #[test]
  fn parse_balls() {
    let input = std::fs::read_to_string("./examples/balls.pas").unwrap();
    assert_eq!(program(input.trim_start()).unwrap().0, "");
  }

  #[test]
  fn parse_hsv_to_rgb() {
    let input = std::fs::read_to_string("./examples/hsv_to_rgb.pas").unwrap();
    assert_eq!(unit(input.trim_start()).unwrap().0, "");
  }
}
