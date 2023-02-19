use std::collections::HashMap;
use std::iter::repeat;
use std::ops::Not;

use dyn_value::DynValue;
use parser::{EntityId, parse};
use parser::binary_operators::BinOp;
use parser::expressions::Expr;
use parser::functions::FnDef;
use parser::statements::Stmt;
use parser::types::{NamedType, Type, VarDef};
use parser::unary_operators::UnOp;
use parser::visitor::Visit;

mod dyn_value;
mod stdlib;

// Interpreter is written in the most ineffective way possible

pub type Scope<'a> = HashMap<&'a str, DynValue<'a>>;

#[derive(Debug, Default)]
pub struct Unit<'a> {
  vars: Scope<'a>,
  funcs: HashMap<&'a str, &'a FnDef<'a>>,
}

#[derive(Debug, Default)]
pub struct Interpreter<'a> {
  units: HashMap<&'a str, Unit<'a>>,
  scopes: Vec<Scope<'a>>,
  stack: Vec<DynValue<'a>>,
  broken_loop: bool,
  current_unit: &'a str,
}

impl<'a> Interpreter<'a> {
  pub fn call_fn(&mut self, id: &'a EntityId, args: &'a Vec<Expr>) {
    // internal function
    if let Some(fun) = stdlib::STDLIB.get(&id) {
      let stack_size = self.stack.len();
      for expr in args {
        self.visit_expr(expr);
      }
      let computed_args = &self.stack[stack_size..];
      let _result: DynValue = fun(computed_args);
      self.stack.truncate(stack_size);
      self.stack.push(_result);
      return;
    }

    // user defined function
    let func = self.find_function(id);

    // check arity
    let arity: usize = func.args.iter().map(|it| it.vars.len()).sum();
    if args.len() != arity {
      panic!("Can't call {}, expected {} arguments, got {}!", func.id, func.args.len(), args.len());
    }

    // put computed arguments on stack
    for expr in args.iter().rev() {
      self.visit_expr(expr);
    }

    // create a scope with variables
    let return_val = self.infer_default_value(&func.result);
    self.scopes.push(Scope::from_iter([(func.id, return_val)]));

    for it in &func.args {
      self.visit_var_def(it); // dyn value
    }
    if let Some(locals) = func.locals.as_ref() {
      self.visit_definition(locals);
    }

    // bind factual arguments
    let def_args =
      func.args.iter().flat_map(|def| def.vars.iter().zip(repeat(&def.of)));

    for (name, _type) in def_args {
      let value = self.stack.pop().unwrap();
      let var = self.scopes.last_mut().unwrap().get_mut(name).unwrap();

      if value.is_assignable(var) {
        *var = value;
      } else {
        panic!("type {} can't be assigned to {}", value.type_name(), var.type_name());
      }
    }

    // call the function
    self.visit_stmt(func.code.as_ref().expect("Expected function body"));

    // return result on stack
    let res = self.scopes.last_mut().unwrap().remove(func.id).unwrap();
    self.stack.push(res);

    self.scopes.pop();
  }

  fn pop(&mut self) -> DynValue<'a> {
    self.stack.pop().unwrap()
  }

  fn unit_mut(&mut self) -> &mut Unit<'a> {
    self.units.entry(self.current_unit).or_insert_with(Unit::default)
  }

  fn find_variable(&mut self, name: &'a EntityId) -> &mut DynValue<'a> {
    match name {
      EntityId::Internal { id } => {
        self
          .scopes
          .iter_mut()
          .rev()
          .take(1)
          .find_map(|sc| sc.get_mut(id))
          .or_else(|| self.units.get_mut(self.current_unit).unwrap().vars.get_mut(id))
          .expect(format!("Variable {id} is not defined").as_str())
      }
      EntityId::External { id, unit } =>
        self
          .units
          .get_mut(unit)
          .expect(format!("Unit {unit} not found").as_str())
          .vars
          .get_mut(id)
          .expect(format!("Variable {id} is not defined").as_str()),
    }
  }

  fn find_function(&mut self, name: &'a EntityId) -> &'a FnDef<'a> {
    match name {
      EntityId::Internal { id } =>
        self.unit_mut().funcs.get(id).expect(format!("Function {id} is not defined").as_str()),
      EntityId::External { id, unit } =>
        self.units.get(unit)
          .expect(format!("Unit {unit} not found").as_str())
          .funcs.get(id).expect(format!("Function {id} is not defined").as_str()),
    }
  }

  fn infer_default_value(&mut self, r#type: &'a Type) -> DynValue<'a> {
    match r#type {
      Type::Named(ref id) => match *id {
        NamedType::External { .. } => {
          panic!("Can't redefine external unit variable");
        }
        NamedType::Internal { id } => match id {
          "string" => DynValue::Str(String::new()),
          "integer" => DynValue::Int(0),
          "real" => DynValue::Float(0.0),
          "bool" => DynValue::Bool(false),
          "void" => DynValue::Void,
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
    }
  }
}

impl<'a> Visit<'a> for Interpreter<'a> {
  fn visit_unit_name(&mut self, name: &'a str) {
    self.current_unit = name;
  }

  fn visit_var_def(&mut self, def: &'a VarDef) {
    let template = self.infer_default_value(&def.of);
    for &var in def.vars.iter() {
      let scope = if let Some(scope) = self.scopes.last_mut() {
        scope
      } else {
        &mut self.unit_mut().vars
      };

      scope.insert(var, template.clone());
    }
  }

  fn visit_fn_definition(&mut self, def: &'a FnDef) {
    self.unit_mut().funcs.insert(def.id, def);
  }

  fn visit_stmt(&mut self, stmt: &'a Stmt) {
    match *stmt {
      Stmt::Block { ref list } => {
        for it in list {
          self.visit_stmt(it);
          if self.broken_loop {
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
        if self.broken_loop {
          self.broken_loop = false;
          return;
        }
      },
      Stmt::Repeat { ref body, ref until } => loop {
        for it in body {
          self.visit_stmt(it);

          if self.broken_loop {
            self.broken_loop = false;
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
        //  - there is a scope in `for` statement (ignore)

        if self.broken_loop {
          self.broken_loop = false;
          return;
        }
        todo!()
      }
      Stmt::FnCall { ref id, ref args } => {
        self.call_fn(id, args);
        self.stack.pop(); // pop return value
      }
      Stmt::Assign { ref ids, ref expr } => {
        if ids.len() != 1 {
          todo!("array access")
        }

        self.visit_expr(expr);
        let value = self.pop();
        let var = self.find_variable(&ids[0].0);

        if value.is_assignable(var) {
          *var = value;
        } else {
          panic!("type {} can't be assigned to {}", value.type_name(), var.type_name());
        }
      }
      Stmt::Break => {
        self.broken_loop = true;
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
          (BinOp::Plus, DynValue::Float(l), DynValue::Float(r)) => DynValue::Float(l + r),
          (BinOp::Minus, DynValue::Float(l), DynValue::Float(r)) => DynValue::Float(l - r),
          (_, _, _) => todo!(),
        };
        self.stack.push(res);
      }
      Expr::VarAccess { ref ids } => {
        if ids.len() != 1 {
          todo!("array access")
        }

        let var_name = &ids[0].0;
        let value = self.find_variable(var_name).clone();
        self.stack.push(value);
      }
      Expr::FnCall { ref id, ref args } => {
        self.call_fn(id, args);
        if self.stack.last().unwrap().is_void() {
          panic!("procedures can't be called inside expressions")
        }
      }
    }
  }
}

fn main() {
  let program = r#"
    program test;

    function A(m, n: integer): integer;
    begin
                  if m = 0 then A := n + 1
       else begin if n = 0 then A := A(m - 1, 1)
                           else A := A(m - 1, A(m, n - 1)) end
    end;

    begin
      assert_eq(A(3, 10), 8189);
      writeln('Correct!');
    end.
  "#;

  let program = parse(program);

  let now = std::time::Instant::now();
  let mut int = Interpreter::default();
  int.visit_module(&program);

  println!("\nfinished in {:.2} secs.", now.elapsed().as_secs_f32());
}

// what's next? hmmm
