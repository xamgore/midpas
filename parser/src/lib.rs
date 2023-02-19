use nom::branch::alt;
use nom::character::complete::{alpha1, alphanumeric1};
use nom::combinator::{all_consuming, opt, recognize};
use nom::multi::{many0_count, separated_list1};
use nom::sequence::pair;
use nom::{Compare, CompareResult, Parser};
use nom_supreme::multi::collect_separated_terminated;
pub use nom_supreme::tag::complete::tag;
use nom_supreme::ParserExt;

use expressions::Expr;
use whitespaces::*;

use crate::expressions::expr;
pub use crate::modules::Module;

pub type IResult<I, O, E = nom_supreme::error::ErrorTree<I>> = Result<(I, O), nom::Err<E>>;

pub mod binary_operators;
pub mod definitions;
pub mod expressions;
pub mod functions;
pub mod literals;
pub mod modules;
pub mod prettier;
pub mod statements;
pub mod types;
pub mod unary_operators;
pub mod visitor;
mod whitespaces;

pub fn parse(source: &str) -> Module {
  all_consuming(modules::module)(source).map(|(_, m)| m).unwrap()
}

// todo: implement expressions interpreter

// todo: move some of these definitions to another file
//       so that we could clean it for the future code
//       (parsing the whole file tree)

// todo: write a visitor that extracts imports from AST
//       these imports then used for loading another files
//       with following parsing
//       caution: keep a set of visited modules
//       a simpler way -> just import all files :)

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EntityId<'a> {
  Internal { id: &'a str },
  External { id: &'a str, unit: &'a str },
}

impl<'a> From<(&'a str, Option<&'a str>)> for EntityId<'a> {
  fn from(value: (&'a str, Option<&'a str>)) -> Self {
    match value {
      (id, None) => EntityId::Internal { id },
      (unit, Some(id)) => EntityId::External { unit, id },
    }
  }
}

// todo:
// todo: consider renaming Def to DefSect

// 0. dynamic pascal interpretation + loading of all the modules together
// 1. type-checking = infer types + have a symbol table
// 2. web assembly
// 3. add more samples
// 4. switch-case operator

// notes:
// - type_section has an extra check for being part of impl or interface section
// - interval types are not allowed inside record declarations (type-check phase)

pub fn chr(c: char) -> impl FnMut(&str) -> IResult<&str, char> {
  move |input: &str| rms0(nom::character::complete::char(c))(input)
}

pub fn keyword(keyword: &'static str) -> impl FnMut(&str) -> IResult<&str, &str> {
  move |input: &str| {
    let (rest, id) = identifier.parse(input)?;
    match id.compare_no_case(keyword) {
      CompareResult::Ok => Ok((rest, id)),
      _ => {
        use nom_supreme::tag::TagError;
        Err(nom::Err::Error(nom_supreme::error::ErrorTree::from_tag(input, keyword)))
      }
    }
  }
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
  rms0(recognize(pair(alt((alpha1, tag("_"))), many0_count(alt((alphanumeric1, tag("_")))))))(input)
}

pub fn entity_id(input: &str) -> IResult<&str, EntityId> {
  let id = identifier;
  let sub = rms0(chr('.').precedes(identifier));
  pair(id, opt(sub)).map(EntityId::from).parse(input)
}

pub fn fn_call(input: &str) -> IResult<&str, (EntityId, Vec<Expr>)> {
  entity_id.and(chr('(').precedes(
    collect_separated_terminated(rms0(expr), chr(','), chr(')'))
      .or(chr(')').value(Vec::new()))
  )).parse(input)
}

pub fn var_access(input: &str) -> IResult<&str, Vec<(EntityId, Option<Vec<Expr>>)>> {
  let var_or_arr_item = pair(
    entity_id,
    opt(chr('[').precedes(collect_separated_terminated(rms0(expr), chr(','), chr(']')))),
  );
  separated_list1(chr('.'), var_or_arr_item)(input)
}
