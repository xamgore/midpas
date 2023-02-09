use derive_more::From;
use nom::branch::alt;
use nom::combinator::success;
use nom::Parser;
use nom::sequence::{pair, tuple};
use nom_supreme::multi::collect_separated_terminated;
use nom_supreme::ParserExt;

use crate::{chr, identifier, IResult, keyword};
use crate::definitions::{Def, var_def};
use crate::statements::{begin_end_stmt, Stmt};
use crate::types::{NamedType, Type, type_expr, VarDef, VOID};
use crate::whitespaces::rms0;

#[derive(Debug, From, Clone, PartialEq)]
pub struct FnDef<'a> {
  pub id: &'a str,
  pub args: Vec<VarDef<'a>>,
  pub result: Type<'a>,
  pub locals: Option<Box<Def<'a>>>,
  pub code: Option<Stmt<'a>>,
}

// > procedure IDN <RD_param_list> ; <RD_proc_block> ; <RD_block>
// > function IDN <RD_param_list> : IDN ; <RD_proc_block> ; <RD_block>
pub fn fn_def(input: &str) -> IResult<&str, FnDef> {
  tuple((
    rms0(keyword("function").or(keyword("procedure"))).precedes(identifier),
    rms0(fn_args),
    alt((
      chr(':').precedes(rms0(type_expr)),
      success(Type::Named(NamedType::Internal { id: VOID })),
    )),
    chr(';').precedes(rms0(var_def.map(Box::new)).opt()),
    alt((
      keyword("forward").value(None).terminated(chr(';')),
      rms0(begin_end_stmt).terminated(chr(';')).opt(),
    )),
  ))
    .map(FnDef::from)
    .parse(input)
}

// > empty
// > "(" [var] <identifier_list> : [IDN.]IDN (; [var] <identifier_list> : [IDN.]IDN )+ ")"
fn fn_args(input: &str) -> IResult<&str, Vec<VarDef>> {
  // var x, y, z :
  let _id_list =
    keyword("var").opt().precedes(collect_separated_terminated(identifier, chr(','), chr(':')));

  chr('(')
    .precedes(collect_separated_terminated(
      pair(_id_list, type_expr).map(VarDef::from),
      chr(';'),
      chr(')'),
    ))
    .or(success(Vec::new()))
    .parse(input)
}

#[cfg(test)]
mod tests {
  use crate::EntityId;

  use super::*;

  #[test]
  fn procedure_test() {
    let input = "procedure p; forward;";
    assert_eq!(
      fn_def(input).unwrap(),
      (
        "",
        FnDef {
          id: "p",
          args: vec![],
          result: Type::Named(NamedType::Internal { id: VOID }),
          locals: None,
          code: None,
        }
      )
    )
  }

  #[test]
  fn procedure_with_body() {
    let input = "procedure p; begin exit; end;";
    assert_eq!(
      fn_def(input).unwrap(),
      (
        "",
        FnDef {
          id: "p",
          args: vec![],
          result: Type::Named(NamedType::Internal { id: VOID }),
          locals: None,
          code: Some(Stmt::Block {
            list: vec![Stmt::FnCall { id: EntityId::Internal { id: "exit" }, args: vec![] }]
          }),
        }
      )
    )
  }

  #[test]
  fn function_with_body() {
    let input = "
      function p :point2D;
      var up, down, pos :integer;
          k: real;
      begin
        exit;
      end;";
    assert_eq!(fn_def(input.trim()).unwrap().0, "")
  }

  #[test]
  fn args_list_test() {
    let input = "(var x, y: int; var b: bool)";
    assert_eq!(
      fn_args(input).unwrap(),
      (
        "",
        vec![
          VarDef { vars: vec!["x", "y"], of: Type::Named(NamedType::Internal { id: "int" }) },
          VarDef { vars: vec!["b"], of: Type::Named(NamedType::Internal { id: "bool" }) },
        ]
      )
    );
  }
}
