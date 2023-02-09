use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::fold_many0;
use nom::Parser;
use nom::sequence::{delimited, pair};
use nom_supreme::ParserExt;

use crate::{chr, EntityId, fn_call, var_access};
use crate::binary_operators::{BinOp, mul_operator, relative_operator, sum_operator};
use crate::IResult;
use crate::literals::{Lit, literal};
use crate::unary_operators::{minus, not, UnOp};
use crate::whitespaces::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
  Literal {
    lit: Lit<'a>,
  },
  Unary {
    op: UnOp,
    expr: Box<Expr<'a>>,
  },
  Binary {
    op: BinOp,
    l: Box<Expr<'a>>,
    r: Box<Expr<'a>>,
  },
  VarAccess {
    /// `id[e, e].id[e, e]`
    ids: Vec<(EntityId<'a>, Option<Vec<Expr<'a>>>)>,
  },
  FnCall {
    id: EntityId<'a>,
    args: Vec<Expr<'a>>,
  },
}

pub fn expr(input: &str) -> IResult<&str, Expr> {
  rel_expr(input)
}

fn rel_expr(input: &str) -> IResult<&str, Expr> {
  pair(sum_expr, opt(relative_operator.and(sum_expr))).map(wrap_binary_opt).parse(input)
}

fn sum_expr(input: &str) -> IResult<&str, Expr> {
  let (input, base) = mul_expr(input)?;

  fold_many0(
    pair(sum_operator, mul_expr),
    move || base.clone(),
    |l, (op, r)| wrap_binary((l, op, r)),
  )(input)
}

fn mul_expr(input: &str) -> IResult<&str, Expr> {
  let (input, base) = not_expr(input)?;

  fold_many0(
    pair(mul_operator, not_expr),
    move || base.clone(),
    |l, (op, r)| wrap_binary((l, op, r)),
  )(input)
}

fn not_expr(input: &str) -> IResult<&str, Expr> {
  map(pair(not.opt(), neg_expr), wrap_unary)(input)
}

fn neg_expr(input: &str) -> IResult<&str, Expr> {
  map(pair(minus.opt(), value_expr), wrap_unary)(input)
}

fn value_expr(input: &str) -> IResult<&str, Expr> {
  rms0(alt((
    map(literal, |lit| Expr::Literal { lit }),
    map(fn_call, |(id, args)| Expr::FnCall { id, args }),
    map(var_access, |ids| Expr::VarAccess { ids }),
    delimited(chr('('), expr, chr(')')),
  )))(input)
}

fn wrap_unary((op, expr): (Option<UnOp>, Expr)) -> Expr {
  match op {
    None => expr,
    Some(op) => Expr::Unary { op, expr: Box::new(expr) },
  }
}

fn wrap_binary<'a>((l, op, r): (Expr<'a>, BinOp, Expr<'a>)) -> Expr<'a> {
  Expr::Binary { l: Box::new(l), op, r: Box::new(r) }
}

fn wrap_binary_opt<'a>((expr, right): (Expr<'a>, Option<(BinOp, Expr<'a>)>)) -> Expr<'a> {
  match right {
    None => expr,
    Some((op, right)) => wrap_binary((expr, op, right)),
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn smoke_test() {
    let input = "25 * 43.2 + 13 div 9 <> not - 1";

    let expected = Expr::Binary {
      l: Box::new(Expr::Binary {
        l: Box::new(Expr::Binary {
          l: Box::new(Expr::Literal { lit: Lit::Int(25) }),
          op: BinOp::Mul,
          r: Box::new(Expr::Literal { lit: Lit::Real(43.2) }),
        }),

        op: BinOp::Plus,
        r: Box::new(Expr::Binary {
          l: Box::new(Expr::Literal { lit: Lit::Int(13) }),
          op: BinOp::IntDiv,
          r: Box::new(Expr::Literal { lit: Lit::Int(9) }),
        }),
      }),

      op: BinOp::NotEqual,
      r: Box::new(Expr::Unary {
        op: UnOp::Not,
        expr: Box::new(Expr::Unary {
          op: UnOp::Minus,
          expr: Box::new(Expr::Literal { lit: Lit::Int(1) }),
        }),
      }),
    };

    assert_eq!(expr(input).unwrap(), ("", expected));
  }

  #[test]
  fn parse_rel_expr() {
    let l = Box::new(Expr::Literal { lit: Lit::Int(1) });
    let r = Box::new(Expr::Literal { lit: Lit::Int(2) });

    let bin = |op: BinOp| Expr::Binary { l: l.clone(), op, r: r.clone() };

    assert_eq!(expr("1 > 2").unwrap(), ("", bin(BinOp::Greater)));
    assert_eq!(expr("1 >= 2").unwrap(), ("", bin(BinOp::GreaterEq)));
    assert_eq!(expr("1 < 2").unwrap(), ("", bin(BinOp::Less)));
    assert_eq!(expr("1 <= 2").unwrap(), ("", bin(BinOp::LessEq)));
    assert_eq!(expr("1 = 2").unwrap(), ("", bin(BinOp::Equal)));
    assert_eq!(expr("1 <> 2").unwrap(), ("", bin(BinOp::NotEqual)));
  }

  #[test]
  fn parse_not_expr() {
    assert_eq!(
      not_expr("not true").unwrap(),
      ("", Expr::Unary { op: UnOp::Not, expr: Box::new(Expr::Literal { lit: Lit::Bool(true) }) })
    );

    // assert_eq!(
    //   not_expr("nottrue").unwrap(),
    //   ("", Expr::VarAccess { ids: vec![(EntityId::Internal { id: "nottrue" }, None)] })
    // );
  }

  #[test]
  fn parse_not_num() {
    assert_eq!(
      expr("not -1").unwrap(),
      (
        "",
        Expr::Unary {
          op: UnOp::Not,
          expr: Box::new(Expr::Unary {
            op: UnOp::Minus,
            expr: Box::new(Expr::Literal { lit: Lit::Int(1) }),
          }),
        }
      )
    );
  }

  #[test]
  fn parse_minus() {
    let expected = Expr::Binary {
      op: BinOp::Minus,
      l: Box::new(Expr::Literal { lit: Lit::Int(1) }),
      r: Box::new(Expr::Literal { lit: Lit::Int(1) }),
    };

    assert_eq!(expr("1-1").unwrap(), ("", expected));
  }

  #[test]
  fn parse_minus_minus1() {
    let expected = Expr::Binary {
      op: BinOp::Minus,
      l: Box::new(Expr::Literal { lit: Lit::Int(1) }),
      r: Box::new(Expr::Unary {
        op: UnOp::Minus,
        expr: Box::new(Expr::Literal { lit: Lit::Int(1) }),
      }),
    };

    assert_eq!(expr("1--1").unwrap(), ("", expected));
  }

  #[test]
  fn parse_braced_expr() {
    assert_eq!(value_expr("(((123)))").unwrap(), ("", Expr::Literal { lit: Lit::Int(123) }));
  }

  #[test]
  fn parse_value_expr() {
    assert_eq!(value_expr("42").unwrap(), ("", Expr::Literal { lit: Lit::Int(42) }));
    assert_eq!(value_expr("2.75").unwrap(), ("", Expr::Literal { lit: Lit::Real(2.75) }));
    assert_eq!(value_expr("true").unwrap(), ("", Expr::Literal { lit: Lit::Bool(true) }));
    // todo: all Lit values
  }
}
