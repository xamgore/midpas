use nom::branch::alt;
use nom::combinator::{map, opt, value};
use nom::Parser;
use nom::sequence::{pair, preceded, tuple};
use nom_supreme::multi::collect_separated_terminated;
use nom_supreme::ParserExt;

use crate::{chr, entity_id, EntityId, fn_call, identifier, IResult, keyword, tag, var_access};
use crate::expressions::{expr, Expr};
use crate::whitespaces::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
  Block {
    list: Vec<Stmt<'a>>,
  },
  // todo: empty?
  If {
    clause: Expr<'a>,
    then: Box<Stmt<'a>>,
    or_else: Box<Stmt<'a>>,
  },
  #[allow(unused)]
  Case,
  While {
    clause: Expr<'a>,
    body: Box<Stmt<'a>>,
  },
  Repeat {
    body: Vec<Stmt<'a>>,
    until: Expr<'a>,
  },
  For {
    var: &'a str,
    init: Expr<'a>,
    inc: bool,
    target: Expr<'a>,
    body: Box<Stmt<'a>>,
  },
  FnCall {
    id: EntityId<'a>,
    args: Vec<Expr<'a>>,
  },
  Assign {
    /// `id[e, e].id[e, e]`
    ids: Vec<(EntityId<'a>, Option<Vec<Expr<'a>>>)>,
    expr: Expr<'a>,
  },
  Break,
}

impl<'a> Default for Stmt<'a> {
  fn default() -> Self {
    Stmt::Block { list: Default::default() }
  }
}

pub fn stmt(input: &str) -> IResult<&str, Stmt> {
  let break_stmt = keyword("break").value(Stmt::Break);

  let fn_call = alt((
    map(fn_call, |(id, args)| Stmt::FnCall { id, args }),
    map(entity_id, |id| Stmt::FnCall { id, args: vec![] }),
  ));

  alt((
    begin_end_stmt,
    while_stmt,
    repeat_stmt,
    for_stmt,
    if_stmt,
    // case_stmt,
    assign_stmt,
    break_stmt,
    fn_call,
  ))(input)
}

pub fn begin_end_stmt(input: &str) -> IResult<&str, Stmt> {
  collect_separated_terminated(
    rms0(stmt),
    chr(';'),
    opt(chr(';')).terminated(keyword("end")),
  )
    .or(keyword("end").value(Vec::new()))
    .preceded_by(keyword("begin"))
    .map(|list| Stmt::Block { list })
    .parse(input)
}

pub fn repeat_stmt(input: &str) -> IResult<&str, Stmt> {
  let _repeat_until = preceded(
    keyword("repeat"),
    collect_separated_terminated(
      rms0(stmt),
      chr(';'),
      chr(';').terminated(keyword("until")),
    )
      .or(keyword("until").value(Vec::new())),
  );

  pair(_repeat_until, expr).map(|(body, until)| Stmt::Repeat { body, until }).parse(input)
}

pub fn while_stmt(input: &str) -> IResult<&str, Stmt> {
  let _while = preceded(keyword("while"), rms0(expr));
  let _do = preceded(keyword("do"), rms0(stmt));
  pair(_while, _do).map(|(clause, body)| Stmt::While { clause, body: Box::new(body) }).parse(input)
}

pub fn if_stmt(input: &str) -> IResult<&str, Stmt> {
  map(
    tuple((
      preceded(keyword("if"), rms0(expr)),
      preceded(keyword("then"), rms0(stmt)),
      opt(preceded(tuple((opt(chr(';')), keyword("else"))), rms0(stmt))),
    )),
    |(clause, then, or_else)| Stmt::If {
      clause,
      then: Box::new(then),
      or_else: Box::new(or_else.unwrap_or_default()),
    },
  )(input)
}

// pub fn case_stmt(_input: &str) -> IResult<&str, Stmt> {
//   // separated_list1(chr(','), literal),
//   // chr(':'),
//   // stmt,
//
//   // tuple((
//   //   keyword("case"),
//   //   rms0(expr),
//   //   keyword("of"),
//   //   todo!(),
//   //   keyword("end"),
//   //   )).map(|(_, expr, _, cases, _)| Stmt::Case)(input)
//   unimplemented!()
// }

pub fn for_stmt(input: &str) -> IResult<&str, Stmt> {
  map(
    tuple((
      keyword("for"),
      identifier,
      rms0(tag(":=")),
      rms0(expr),
      rms0(alt((value(true, keyword("to")), value(false, keyword("downto"))))),
      rms0(expr),
      keyword("do"),
      rms0(stmt),
    )),
    |(_, var, _, init, inc, target, _, body)| Stmt::For {
      var,
      init,
      inc,
      target,
      body: Box::new(body),
    },
  )(input)
}

pub fn assign_stmt(input: &str) -> IResult<&str, Stmt> {
  pair(rms0(var_access), rms0(tag(":=")).precedes(rms0(expr)))
    .map(|(ids, expr)| Stmt::Assign { ids, expr })
    .parse(input)
}

#[cfg(test)]
mod tests {
  use crate::binary_operators::BinOp;
  use crate::literals::Lit;

  use super::*;

  #[test]
  fn test_fn_call_stmt() {
    assert_eq!(
      stmt(r#"f"#).unwrap(),
      ("", Stmt::FnCall { id: EntityId::Internal { id: "f" }, args: vec![] })
    );
    assert_eq!(
      stmt(r#"f "#).unwrap(),
      ("", Stmt::FnCall { id: EntityId::Internal { id: "f" }, args: vec![] })
    );
    assert_eq!(
      stmt(r#"f;"#).unwrap(),
      (";", Stmt::FnCall { id: EntityId::Internal { id: "f" }, args: vec![] })
    );
  }

  #[test]
  fn test_break_stmt() {
    assert_eq!(stmt(r#"break"#).unwrap(), ("", Stmt::Break));
    assert_eq!(stmt(r#"break "#).unwrap(), ("", Stmt::Break));
    assert_eq!(stmt(r#"break;"#).unwrap(), (";", Stmt::Break));
  }

  #[test]
  fn test_empty_block_stmt() {
    assert_eq!(begin_end_stmt(r#"begin end"#).unwrap(), ("", Stmt::Block { list: vec![] }));
  }

  #[test]
  fn test_non_empty_block_stmt() {
    assert_eq!(
      begin_end_stmt(r#"begin break; end"#).unwrap(),
      ("", Stmt::Block { list: vec![Stmt::Break] })
    )
  }

  #[test]
  fn test_empty_repeat_stmt() {
    let input = r#"repeat until true"#;
    let expected = Stmt::Repeat { body: vec![], until: Expr::Literal { lit: Lit::Bool(true) } };

    assert_eq!(repeat_stmt(input).unwrap(), ("", expected));
  }

  #[test]
  fn test_repeat_stmt() {
    let input = r#"repeat break; until true"#;
    let expected =
      Stmt::Repeat { body: vec![Stmt::Break], until: Expr::Literal { lit: Lit::Bool(true) } };

    assert_eq!(repeat_stmt(input).unwrap(), ("", expected));
  }

  #[test]
  fn test_block_repeat_stmt() {
    let input = r#"begin repeat break; until true; end"#;
    let expected = Stmt::Block {
      list: vec![Stmt::Repeat {
        body: vec![Stmt::Break],
        until: Expr::Literal { lit: Lit::Bool(true) },
      }],
    };

    assert_eq!(begin_end_stmt(input).unwrap(), ("", expected));
  }

  #[test]
  fn test_while_stmt() {
    assert_eq!(
      stmt(r#"while true do begin break; end"#).unwrap(),
      (
        "",
        Stmt::While {
          clause: Expr::Literal { lit: Lit::Bool(true) },
          body: Box::new(Stmt::Block { list: vec![Stmt::Break] }),
        }
      )
    );
  }

  #[test]
  fn test_while2_stmt() {
    assert_eq!(
      stmt(r#"while true do begin break; end;"#).unwrap(),
      (
        ";",
        Stmt::While {
          clause: Expr::Literal { lit: Lit::Bool(true) },
          body: Box::new(Stmt::Block { list: vec![Stmt::Break] }),
        }
      )
    );
  }

  #[test]
  fn real_block_while_test() {
    let input = r#"
    begin
      i:=5;
      While true do
        begin
        break;
        end;
      break;
    end
    "#;

    assert_eq!(begin_end_stmt(input.trim()).unwrap().0, ""); // todo
  }

  #[test]
  fn test_if_stmt() {
    assert_eq!(
      stmt(r#"if GameResult=1 then break else break"#).unwrap(),
      (
        "",
        Stmt::If {
          clause: Expr::Binary {
            op: BinOp::Equal,
            l: Box::new(Expr::VarAccess {
              ids: vec![(EntityId::Internal { id: "GameResult" }, None, )]
            }),
            r: Box::new(Expr::Literal { lit: Lit::Int(1) }),
          },
          then: Box::new(Stmt::Break),
          or_else: Box::new(Stmt::Break),
        }
      )
    )
  }

  #[test]
  fn test_for_stmt() {
    assert_eq!(stmt(r#"begin for i:=2 downto 1 do break; end"#.trim()).unwrap().0, "")
  }

  #[test]
  fn if_with_two_branches_and_semicolon() {
    let f = r#"
      begin
        if true then
          break;
        else
          break;
      end
    "#;

    assert_eq!(begin_end_stmt(f.trim()).unwrap().0, "");
  }

  #[test]
  fn if_with_single_branch() {
    let f = r#"
      begin
        if true then break;
      end
    "#;

    assert_eq!(begin_end_stmt(f.trim()).unwrap().0, "");
  }

  #[test]
  fn test() {
    let f = r#"
      begin
        break
      end
    "#;

    assert_eq!(begin_end_stmt(f.trim()).unwrap().0, "");
  }

  // todo: if branching problem
}
