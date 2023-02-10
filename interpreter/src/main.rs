mod dyn_value;

use std::collections::HashMap;
use std::ops::Not;
use dyn_value::DynValue;

use parser::binary_operators::BinOp;
use parser::definitions::Def;
use parser::expressions::Expr;
use parser::statements::Stmt;
use parser::types::{NamedType, Type};
use parser::unary_operators::UnOp;
use parser::visitor::Visit;
use parser::{EntityId, parse};

// Interpreter is written in the most ineffective way possible

#[derive(Default)]
pub struct Interpreter<'a> {
  stack: Vec<DynValue<'a>>,
  broken: bool,

  // todo: variables
  //  simple case: just make a dict: name -> DynValue
  vars: HashMap<&'a str, DynValue<'a>>,
}

impl<'a> Interpreter<'a> {
  fn pop(&mut self) -> DynValue<'a> {
    self.stack.pop().unwrap()
  }
}

impl<'a> Visit<'a> for Interpreter<'a> {
  fn visit_definition(&mut self, def_sect: &'a Def) {
    match *def_sect {
      Def::Vars(ref list) => {
        for def in list {
          let template = match def.of {
            Type::Named(ref id) => match *id {
              NamedType::External { .. } => {
                panic!()
              }
              NamedType::Internal { id } => match id {
                "string" => DynValue::Str(String::new()),
                "integer" => DynValue::Int(0),
                "real" => DynValue::Float(0.0),
                "bool" => DynValue::Bool(false),
                _ => panic!(),
              },
            },
            Type::Range(_) => {
              panic!()
            }
            Type::File(_) => {
              panic!()
            }
            Type::Array(_) => {
              panic!()
            }
            Type::Record(_) => {
              panic!()
            }
          };
          for &var in def.vars.iter() {
            self.vars.insert(var, template.clone());
          }
        }
      }
      Def::Consts(_) => {}
      Def::Fn(_) => {}
      Def::Types(_) => {}
    }
  }

  fn visit_stmt(&mut self, stmt: &'a Stmt) {
    match *stmt {
      Stmt::Block { ref list } => {
        for it in list {
          self.visit_stmt(it);
          if self.broken {
            return;
          }
        }
      }
      Stmt::If { ref clause, ref then, ref or_else } => {
        self.visit_expr(clause);
        if let DynValue::Bool(val) = self.pop() {
          self.visit_stmt(if val { then } else { or_else });
        } else {
          panic!("clause in if statement must have boolean type");
        }
      }
      Stmt::Case => {
        todo!()
      }
      Stmt::While { ref clause, ref body } => loop {
        self.visit_expr(clause);
        if let DynValue::Bool(val) = self.pop() {
          if !val {
            break;
          }
        } else {
          panic!("clause in while loop must have boolean type");
        }

        self.visit_stmt(body);
        if self.broken {
          self.broken = false;
          return;
        }
      },
      Stmt::Repeat { ref body, ref until } => loop {
        for it in body {
          self.visit_stmt(it);

          if self.broken {
            self.broken = false;
            return;
          }
        }

        self.visit_expr(until);
        if let DynValue::Bool(val) = self.pop() {
          if val {
            break;
          }
        } else {
          panic!("clause in repeat-until must have boolean type");
        }
      },
      Stmt::For { .. } => {
        if self.broken {
          self.broken = false;
          return;
        }
        todo!()
      }
      Stmt::FnCall { ref id, ref args } => {
        assert_eq!(*id, EntityId::Internal { id: "writeln" });
        for arg in args {
          self.visit_expr(arg);
          let val = self.stack.pop().unwrap();
          print!("{}", val.to_string());
        }
        println!();
      }
      Stmt::Assign { ref ids, ref expr } => {
        if ids.len() == 1 {
          if let EntityId::Internal { id } = ids[0].0 {
            self.visit_expr(expr);
            let value = self.pop();

            match self.vars.get(id) {
              None => panic!("variable {} is not defined", id),
              Some(var) => {
                if value.is_assignable(var) {
                  self.vars.insert(id, value);
                  return;
                } else {
                  panic!("type {} can't be assigned to {}", value.type_name(), var.type_name());
                }
              }
            }
          }

          todo!("external modules");
        }

        todo!("array access")
      }
      Stmt::Break => {
        self.broken = true;
      }
    }
  }

  fn visit_expr(&mut self, expr: &'a Expr) {
    match *expr {
      Expr::Literal { ref lit } => {
        self.stack.push(lit.into());
      }
      Expr::Unary { ref op, ref expr } => {
        self.visit_expr(expr);
        let res = match (op, self.stack.pop().unwrap()) {
          (UnOp::Not, DynValue::Bool(val)) => DynValue::Bool(!val),
          (UnOp::Not, DynValue::Int(val)) => DynValue::Int(val.not()),
          (UnOp::Not, val) => panic!("expected boolean or int, got {}", val.type_name()),
          (UnOp::Minus, DynValue::Int(val)) => DynValue::Int(-val),
          (UnOp::Minus, DynValue::Float(val)) => DynValue::Float(-val),
          (UnOp::Minus, val) => panic!("expected int or float, got {}", val.type_name()),
        };
        self.stack.push(res);
      }
      Expr::Binary { ref l, op, ref r } => {
        self.visit_expr(l);
        self.visit_expr(r);
        let r = self.stack.pop().unwrap();
        let l = self.stack.pop().unwrap();
        let res = match (op, l, r) {
          // int
          (BinOp::Less, DynValue::Int(l), DynValue::Int(r)) => DynValue::Bool(l < r),
          (BinOp::LessEq, DynValue::Int(l), DynValue::Int(r)) => DynValue::Bool(l <= r),
          (BinOp::Greater, DynValue::Int(l), DynValue::Int(r)) => DynValue::Bool(l > r),
          (BinOp::GreaterEq, DynValue::Int(l), DynValue::Int(r)) => DynValue::Bool(l >= r),
          (BinOp::Equal, DynValue::Int(l), DynValue::Int(r)) => DynValue::Bool(l == r),
          (BinOp::NotEqual, DynValue::Int(l), DynValue::Int(r)) => DynValue::Bool(l != r),
          (BinOp::Plus, DynValue::Int(l), DynValue::Int(r)) => DynValue::Int(l + r),
          (BinOp::Minus, DynValue::Int(l), DynValue::Int(r)) => DynValue::Int(l - r),
          (BinOp::Mul, DynValue::Int(l), DynValue::Int(r)) => DynValue::Int(l * r),
          (BinOp::Div, DynValue::Int(l), DynValue::Int(r)) => DynValue::Float(l as f32 / r as f32),
          (BinOp::IntDiv, DynValue::Int(l), DynValue::Int(r)) => DynValue::Int(l / r),
          (BinOp::IntMod, DynValue::Int(l), DynValue::Int(r)) => DynValue::Int(l % r),
          (BinOp::ShiftLeft, DynValue::Int(l), DynValue::Int(r)) => DynValue::Int(l << r),
          (BinOp::ShiftRight, DynValue::Int(l), DynValue::Int(r)) => DynValue::Int(l >> r),
          (BinOp::UShiftRight, DynValue::Int(l), DynValue::Int(r)) => {
            DynValue::Int(((l as u64) >> r) as i64)
          }
          (BinOp::And, DynValue::Int(l), DynValue::Int(r)) => DynValue::Int(l & r),
          (BinOp::Or, DynValue::Int(l), DynValue::Int(r)) => DynValue::Int(l | r),
          (BinOp::Xor, DynValue::Int(l), DynValue::Int(r)) => DynValue::Int(l ^ r),
          // bool
          (BinOp::And, DynValue::Bool(l), DynValue::Bool(r)) => DynValue::Bool(l && r),
          (BinOp::Or, DynValue::Bool(l), DynValue::Bool(r)) => DynValue::Bool(l || r),
          (BinOp::Xor, DynValue::Bool(l), DynValue::Bool(r)) => DynValue::Bool(l != r),
          // strings
          (BinOp::Plus, DynValue::Str(ref l), DynValue::Str(ref r)) => DynValue::Str({
            let mut s = l.clone();
            s += r.as_str();
            s
          }),
          (BinOp::Less, DynValue::Str(ref l), DynValue::Str(ref r)) => {
            DynValue::Bool(l.cmp(r).is_lt())
          }
          (BinOp::LessEq, DynValue::Str(ref l), DynValue::Str(ref r)) => {
            DynValue::Bool(l.cmp(r).is_le())
          }
          (BinOp::Greater, DynValue::Str(ref l), DynValue::Str(ref r)) => {
            DynValue::Bool(l.cmp(r).is_gt())
          }
          (BinOp::GreaterEq, DynValue::Str(ref l), DynValue::Str(ref r)) => {
            DynValue::Bool(l.cmp(r).is_ge())
          }
          (BinOp::Equal, DynValue::Str(ref l), DynValue::Str(ref r)) => {
            DynValue::Bool(l.cmp(r).is_eq())
          }
          (BinOp::NotEqual, DynValue::Str(ref l), DynValue::Str(ref r)) => {
            DynValue::Bool(l.cmp(r).is_ne())
          }
          // char
          // float
          (_, _, _) => todo!(),
        };
        self.stack.push(res);
      }
      Expr::VarAccess { ref ids } => {
        if ids.len() == 1 {
          if let EntityId::Internal { id } = ids[0].0 {
            match self.vars.get(id) {
              None => panic!("variable {} is not defined", id),
              Some(var) => self.stack.push(var.clone()),
            }
            return;
          }

          todo!("external modules");
        }

        todo!("array access")
      }
      Expr::FnCall { .. } => {
        todo!()

        // if return value is void then panic!("procedures can't be called inside expressions"),
      }
    }
  }
}

fn main() {
  let program = r#"
    program test;

    var
      x, y: integer;

    begin
      while x < 10 do
      begin
        writeln("x: ", x, ", y: ", y);
        x := x + 1;
      end;
    end.
  "#;

  let program = parse(program);

  let mut int = Interpreter::default();
  int.visit_module(&program);

  println!("\nfinished.");
  println!("{{ stack: {:?} }}", int.stack);
}

// what's next? hmmm
