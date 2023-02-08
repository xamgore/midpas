use nom::branch::alt;
use nom::character::complete::{alpha1, alphanumeric1};
use nom::combinator::{opt, recognize};
use nom::multi::{many0_count, separated_list1};
use nom::sequence::pair;
use nom::{Compare, CompareResult, Parser};
use nom_supreme::multi::collect_separated_terminated;
pub use nom_supreme::tag::complete::tag;
use nom_supreme::ParserExt;

use expressions::Expr;
use whitespaces::*;

use crate::expressions::expr;

pub type IResult<I, O, E = nom_supreme::error::ErrorTree<I>> = Result<(I, O), nom::Err<E>>;

mod binary_operators;
mod definitions;
mod expressions;
mod functions;
mod literals;
mod modules;
mod prettier;
mod statements;
mod types;
mod unary_operators;
mod whitespaces;
mod visitor;

#[derive(Debug, Clone, PartialEq)]
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
// 2. pretty-printer (visitor)

//

// first idea: let's just make a recursive procedure, that will be calling itself
// there will be a context with { spaces: 2 }
// but these functions should be at AST nodes, so that must be a trait
// and each of the nodes implements those traits
// nah

// the second idea (somewhere ~visitors)
// there is a trait Visitor
// there is a trait implementation
// AST nodes have a method accept(visitor)
// each AST node implements trait Visitor (means it can be traversed)

// 3. better error reports
// 5. add more samples

// 6. replace whitespaces with rms0
// 8. type-checking

// 9. switch-case operator

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
  entity_id
    .and(chr('(').precedes(collect_separated_terminated(rms0(expr), chr(','), chr(')'))))
    .parse(input)
}

pub fn var_access(input: &str) -> IResult<&str, Vec<(EntityId, Option<Vec<Expr>>)>> {
  let var_or_arr_item = pair(
    entity_id,
    opt(chr('[').precedes(collect_separated_terminated(rms0(expr), chr(','), chr(']')))),
  );
  separated_list1(chr('.'), var_or_arr_item)(input)
}
