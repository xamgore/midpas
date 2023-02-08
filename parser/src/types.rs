use derive_more::From;
use nom::branch::alt;
use nom::combinator::opt;
use nom::Parser;
use nom::sequence::{pair, separated_pair, tuple};
use nom_supreme::multi::collect_separated_terminated;
use nom_supreme::ParserExt;

use literals::{char_lit, int_lit};

use crate::{entity_id, IResult};
use crate::{chr, EntityId, identifier, keyword, literals, tag};
use crate::literals::Lit;
use crate::whitespaces::*;

pub const VOID: &str = "void";

pub type NamedType<'a> = EntityId<'a>;

#[derive(Debug, Clone, PartialEq)]
pub enum ConstVal<'a> {
  // int 5, char 'x'
  Lit(Lit<'a>),
  // constant value holding a literal
  Id(EntityId<'a>),
}

// may be like [5..10] or [WIDTH..HEIGHT]
#[derive(Debug, Clone, PartialEq)]
pub struct RangeType<'a> {
  pub start: ConstVal<'a>,
  pub end: ConstVal<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Segment<'a> {
  Range(RangeType<'a>),
  // to a range type
  Id(NamedType<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FileType<'a> {
  pub of: NamedType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType<'a> {
  pub packed: bool,
  pub sizes: Vec<Segment<'a>>,
  pub of: Box<Type<'a>>,
}

#[derive(Debug, From, Clone, PartialEq)]
pub struct VarDef<'a> {
  pub vars: Vec<&'a str>,
  pub of: Type<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordType<'a> {
  pub fields: Vec<VarDef<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
  Named(NamedType<'a>),
  Range(RangeType<'a>),
  File(FileType<'a>),
  Array(ArrayType<'a>),
  Record(RecordType<'a>),
}

pub fn type_expr(input: &str) -> IResult<&str, Type> {
  rms0(alt((
    range_type.map(Type::Range),
    file_type.map(Type::File),
    array_type.map(Type::Array),
    record_type.map(Type::Record),
    named_type.map(Type::Named), // should be the last
  )))(input)
}

pub fn named_type(input: &str) -> IResult<&str, NamedType> {
  entity_id(input)
}

pub fn range_type(input: &str) -> IResult<&str, RangeType> {
  fn const_val(input: &str) -> IResult<&str, ConstVal> {
    let id = identifier.map(|id| ConstVal::Id(EntityId::Internal { id }));
    let lit = int_lit.or(char_lit).map(|lit| ConstVal::Lit(lit));
    alt((id, rms0(lit))).parse(input)
  }

  separated_pair(const_val, rms0(tag("..")), const_val)
    .map(|(start, end)| RangeType { start, end })
    .parse(input)
}

pub fn file_type(input: &str) -> IResult<&str, FileType> {
  keyword("file")
    .terminated(keyword("of"))
    .precedes(named_type)
    .map(|id| FileType { of: id })
    .parse(input)
}

// array [0..3, range2] of image;
pub fn array_type(input: &str) -> IResult<&str, ArrayType> {
  tuple((
    opt(keyword("packed")).map(|opt| opt.is_some()),
    keyword("array"),
    chr('['),
    collect_separated_terminated(
      rms0(alt((named_type.map(Segment::Id), range_type.map(Segment::Range)))),
      chr(','),
      chr(']'),
    ),
    keyword("of"),
    type_expr.map(Box::new),
  ))
    .map(|(packed, _, _, sizes, _, of)| ArrayType { packed, sizes, of })
    .parse(input)
}

// record x,y:int; b:bool; end
pub fn record_type(input: &str) -> IResult<&str, RecordType> {
  let _ids = collect_separated_terminated(identifier, chr(','), chr(':'));

  keyword("record")
    .precedes(collect_separated_terminated(
      pair(_ids, type_expr).map(VarDef::from),
      chr(';'),
      chr(';').terminated(keyword("end")),
    ))
    .map(|fields| RecordType { fields })
    .parse(input)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_named_type() {
    assert_eq!(named_type("ty").unwrap(), ("", EntityId::Internal { id: "ty" }));
    assert_eq!(
      named_type("module.ty").unwrap(),
      ("", EntityId::External { id: "ty", unit: "module" })
    );
  }

  #[test]
  fn test_range_type() {
    assert_eq!(
      range_type("1..5").unwrap(),
      ("", RangeType { start: ConstVal::Lit(Lit::Int(1)), end: ConstVal::Lit(Lit::Int(5)) })
    );

    assert_eq!(
      range_type("#65..#$41").unwrap(),
      (
        "",
        RangeType { start: ConstVal::Lit(Lit::Char('A')), end: ConstVal::Lit(Lit::CharHex('A')) }
      )
    );

    assert_eq!(
      range_type("id..id").unwrap(),
      (
        "",
        RangeType {
          start: ConstVal::Id(EntityId::Internal { id: "id" }),
          end: ConstVal::Id(EntityId::Internal { id: "id" }),
        }
      )
    );
  }

  #[test]
  fn test_array_type() {
    assert_eq!(
      array_type("array [1..maxWay2] of point2d").unwrap(),
      (
        "",
        ArrayType {
          packed: false,
          sizes: vec![Segment::Range(RangeType {
            start: ConstVal::Lit(Lit::Int(1)),
            end: ConstVal::Id(EntityId::Internal { id: "maxWay2" }),
          })],
          of: Box::new(Type::Named(EntityId::Internal { id: "point2d" })),
        }
      )
    );
  }

  #[test]
  fn test_file_type() {
    assert_eq!(
      file_type("file of int").unwrap(),
      ("", FileType { of: EntityId::Internal { id: "int" } })
    );
  }

  #[test]
  fn test_record_type() {
    let f = "record
				tip:integer;
				x,y:real;
				end";

    assert_eq!(
      record_type(f).unwrap(),
      (
        "",
        RecordType {
          fields: vec![
            VarDef { vars: vec!["tip"], of: Type::Named(EntityId::Internal { id: "integer" }) },
            VarDef { vars: vec!["x", "y"], of: Type::Named(EntityId::Internal { id: "real" }) },
          ]
        }
      )
    );
  }
}
