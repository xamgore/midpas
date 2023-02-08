use itertools::Itertools;

use crate::binary_operators::BinOp;
use crate::definitions::{ConstDef, Def, TypeDef};
use crate::expressions::Expr;
use crate::literals::Lit;
use crate::modules::{Module, Section};
use crate::statements::Stmt;
use crate::types::{RecordType, Segment, Type, VarDef, VOID};
use crate::unary_operators::UnOp;
use crate::visitor::{
  walk_const_def, walk_definition, walk_impl_section, walk_intf_section, walk_module,
  walk_type_def, walk_var_def, Visit,
};
use crate::EntityId;

#[derive(Default)]
pub struct Prettier {
  space: String,
  is_unit: bool,
  is_func_def: usize,
}

impl Prettier {
  pub fn inc(&mut self) {
    self.space.push_str("  ");
  }

  pub fn dec(&mut self) {
    self.space.pop();
    self.space.pop();
  }

  pub fn offset(&self) {
    print!("{}", self.space);
  }
}

impl Prettier {}

#[allow(unstable_name_collisions)]
impl Visit<'_> for Prettier {
  fn visit_module(&mut self, m: &'_ Module) {
    match *m {
      Module::Program(ref program) => {
        print!("program {};\n", program.name);
        walk_module(self, m);
        print!(".\n");
      }
      Module::Unit(ref unit) => {
        self.is_unit = true;
        print!("unit {};\n\n", unit.name);
        walk_module(self, m);
        print!("end.\n");
      }
    }
  }

  fn visit_intf_sect(&mut self, sect: &'_ Section) {
    print!("interface\n");
    walk_intf_section(self, sect);
    println!();
  }

  fn visit_impl_sect(&mut self, i: &'_ Section) {
    if self.is_unit {
      print!("implementation\n");
    }
    walk_impl_section(self, i);
    println!();
  }

  fn visit_imports(&mut self, imports: &'_ Vec<&str>) {
    if !imports.is_empty() {
      self.inc();

      print!("\nuses\n{}", self.space);
      let sep = format!(",\n{}", self.space);

      imports.iter().intersperse(&sep.as_str()).chain([&";\n"]).for_each(|p| print!("{p}"));

      println!();
      self.dec();
    }
  }

  fn visit_definition(&mut self, def_sect: &'_ Def) {
    self.offset();

    match *def_sect {
      Def::Vars(ref list) => {
        println!("var");
        self.inc();
        for it in list {
          self.visit_var_def(it);
          print!(";\n");
        }
        self.dec();
      }
      Def::Consts(_) => {
        println!("const");
        self.inc();
        walk_definition(self, def_sect);
        self.dec();
      }
      Def::Types(_) => {
        println!("type");
        self.inc();
        walk_definition(self, def_sect);
        self.dec();
      }
      Def::Fn(ref func) => {
        self.is_func_def += 1;

        {
          let returns_void = matches!(func.result, Type::Named(EntityId::Internal { id: VOID }),);

          if returns_void {
            print!("procedure ");
          } else {
            print!("function ");
          }

          print!("{}", func.id);

          if !func.args.is_empty() {
            print!("(");
            for it in &func.args[..func.args.len().saturating_sub(1)] {
              self.visit_var_def(it);
              print!("; ");
            }
            self.visit_var_def(func.args.last().unwrap());
            print!(")");
          }

          if !returns_void {
            print!(": ");
            self.visit_type(&func.result);
          }

          print!(";\n");

          if let Some(locals) = func.locals.as_ref() {
            self.visit_definition(locals);
          }
          if let Some(code) = func.code.as_ref() {
            self.visit_stmt(code);
            print!(";\n");
          }
        }

        self.is_func_def -= 1;
      }
    }

    if self.is_func_def == 0 {
      println!();
    }
  }

  // todo: var println!(";");
  fn visit_var_def(&mut self, var_def: &'_ VarDef) {
    self.offset();
    for it in var_def.vars.iter().intersperse(&", ") {
      print!("{it}");
    }
    print!(": ");
    walk_var_def(self, var_def);
  }

  fn visit_const_def(&mut self, const_def: &'_ ConstDef) {
    print!("{}{} = ", self.space, const_def.0);
    walk_const_def(self, const_def);
    println!(";");
  }

  fn visit_type_def(&mut self, type_def: &'_ TypeDef) {
    print!("{}{} = ", self.space, type_def.0);
    walk_type_def(self, type_def);
    println!(";");
  }

  fn visit_stmt(&mut self, stmt: &'_ Stmt) {
    match *stmt {
      Stmt::Block { ref list } => {
        self.dec();
        println!("{}begin", self.space);
        self.inc();
        for it in list {
          self.visit_stmt(it);
          println!(";");
        }
        self.dec();
        print!("{}end", self.space);
        if !self.space.is_empty() {
          self.inc();
        }
      }
      Stmt::If { ref clause, ref then, ref or_else } => {
        print!("{}if ", self.space);
        self.visit_expr(clause);
        print!(" then\n");
        self.inc();
        self.visit_stmt(then);
        self.dec();

        let has_empty_branch = matches!(**or_else,Stmt::Block { ref list } if list.is_empty());
        if !has_empty_branch {
          print!("\n{}else\n", self.space);
          self.inc();
          self.visit_stmt(or_else);
          self.dec();
        }
      }
      Stmt::Case => {
        todo!()
      }
      Stmt::While { ref clause, ref body } => {
        self.offset();
        print!("while ");
        self.visit_expr(clause);
        print!(" do\n");
        self.inc();
        self.visit_stmt(body);
        self.dec();
      }
      Stmt::Repeat { ref body, ref until } => {
        self.offset();
        print!("repeat\n");
        self.inc();
        for it in body {
          self.visit_stmt(it);
          print!(";\n");
        }
        self.dec();
        self.offset();
        print!("until ");
        self.visit_expr(until);
      }
      Stmt::For { var, ref init, inc, ref target, ref body } => {
        self.offset();
        print!("for {var} := ");
        self.visit_expr(init);
        print!(" {} ", if inc { "to" } else { "downto" });
        self.visit_expr(target);
        print!(" do\n");
        self.inc();
        self.visit_stmt(body);
        self.dec();
      }
      Stmt::FnCall { ref id, ref args } => {
        self.offset();
        self.visit_entity_id(id);
        if !args.is_empty() {
          print!("(");
          let mut first = true;
          for it in args {
            if !first {
              print!(", ");
            }
            first = false;
            self.visit_expr(it);
          }
          print!(")");
        }
      }
      Stmt::Assign { ref ids, ref expr } => {
        self.offset();
        let var_acc = Expr::VarAccess { ids: ids.clone() };
        self.visit_expr(&var_acc);
        print!(" := ");
        self.visit_expr(expr);
      }
      Stmt::Break => {
        self.offset();
        print!("break");
      }
    }
  }

  fn visit_expr(&mut self, expr: &'_ Expr) {
    match *expr {
      Expr::Literal { ref lit } => {
        self.visit_lit(lit);
      }
      Expr::Unary { op, ref expr } => {
        match op {
          UnOp::Not => print!("not "),
          UnOp::Minus => print!("-"),
        };
        self.visit_expr(expr);
      }
      Expr::Binary { ref l, op, ref r } => {
        self.visit_expr(l);
        // todo: what about brackets?
        match op {
          BinOp::Less => print!(" < "),
          BinOp::LessEq => print!(" <= "),
          BinOp::Greater => print!(" > "),
          BinOp::GreaterEq => print!(" >= "),
          BinOp::Equal => print!(" = "),
          BinOp::NotEqual => print!(" <> "),
          BinOp::Plus => print!(" + "),
          BinOp::Minus => print!(" - "),
          BinOp::And => print!(" and "),
          BinOp::Or => print!(" or "),
          BinOp::Xor => print!(" xor "),
          BinOp::Mul => print!(" * "),
          BinOp::Div => print!(" / "),
          BinOp::UShiftRight => print!(" ushr "),
          BinOp::ShiftRight => print!(" shr "),
          BinOp::ShiftLeft => print!(" shl "),
          BinOp::IntDiv => print!(" div "),
          BinOp::IntMod => print!(" mod "),
        }
        self.visit_expr(r);
      }
      Expr::VarAccess { ref ids } => {
        let mut first = true;
        for (id, indexes) in ids {
          if !first {
            print!(".");
          }
          first = false;

          self.visit_entity_id(id);
          if let Some(indexes) = indexes.as_ref() {
            print!("[");
            for idx in &indexes[..indexes.len().saturating_sub(1)] {
              self.visit_expr(idx);
              print!(", ");
            }
            if let Some(last) = indexes.last() {
              self.visit_expr(last);
            }
            print!("]");
          }
        }
      }
      Expr::FnCall { ref id, ref args } => {
        self.visit_entity_id(id);
        if !args.is_empty() {
          print!("(");
          let mut first = true;
          for it in args {
            if !first {
              print!(", ");
            }
            first = false;
            self.visit_expr(it);
          }
          print!(")");
        }
      }
    }
  }

  fn visit_type(&mut self, ty: &'_ Type) {
    match *ty {
      Type::Named(ref id) => {
        self.visit_entity_id(id);
      }
      Type::Range(ref ty) => {
        self.visit_const_val(&ty.start);
        print!("..");
        self.visit_const_val(&ty.end);
      }
      Type::File(ref ty) => {
        print!("file of ");
        self.visit_entity_id(&ty.of);
      }
      Type::Array(ref arr) => {
        if arr.packed {
          print!("packed ");
        }
        print!("array ");

        for size in &arr.sizes {
          self.visit_segment(size);
        }

        print!(" of ");
        self.visit_type(&arr.of);
      }
      Type::Record(RecordType { ref fields }) => {
        print!("record\n");
        self.inc();
        for it in fields {
          self.visit_var_def(&it);
          println!(";");
        }
        self.dec();
        print!("{}end", self.space);
      }
    }
  }

  // todo
  fn visit_lit(&mut self, lit: &'_ Lit) {
    match *lit {
      Lit::IntHex(x) => {
        print!("{x}");
      }
      Lit::Int(x) => {
        print!("{x}");
      }
      Lit::Real(x) => {
        print!("{x}");
      }
      Lit::Bool(x) => {
        print!("{x}");
      }
      Lit::CharHex(x) => {
        print!("{x}");
      }
      Lit::Char(x) => {
        print!("'{x}'");
      }
      Lit::Str(x) => {
        print!("'{x}'");
      }
    }
  }

  fn visit_entity_id(&mut self, id: &'_ EntityId) {
    match *id {
      EntityId::Internal { id } => {
        print!("{id}");
      }
      EntityId::External { id, unit } => {
        print!("{unit}.{id}");
      }
    }
  }

  fn visit_segment(&mut self, segment: &'_ Segment) {
    print!("[");
    match *segment {
      Segment::Range(ref ty) => {
        self.visit_const_val(&ty.start);
        print!("..");
        self.visit_const_val(&ty.end);
      }
      Segment::Id(ref id) => {
        self.visit_entity_id(id);
      }
    }
    print!("]");
  }
}

#[cfg(test)]
mod tests {
  use crate::modules::module;

  use super::*;

  #[test]
  fn pretty_print_hsv_to_rgb() {
    let _input = std::fs::read_to_string("./examples/hsv_to_rgb.pas").unwrap();
    let _input = std::fs::read_to_string("./examples/balls.pas").unwrap();
    let module = module(_input.trim_start()).unwrap().1;

    let mut prettier = Prettier::default();
    prettier.visit_module(&module);
  }
}
