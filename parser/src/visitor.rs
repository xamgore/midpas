use crate::definitions::{ConstDef, Def, TypeDef};
use crate::EntityId;
use crate::expressions::Expr;
use crate::functions::FnDef;
use crate::literals::Lit;
use crate::modules::{Module, Program, Section, Unit};
use crate::statements::Stmt;
use crate::types::{ConstVal, Segment, Type, VarDef};

#[allow(unused_variables)]
pub trait Visit<'ast> {
  fn visit_module(&mut self, i: &'ast Module) {
    walk_module(self, i);
  }

  fn visit_intf_sect(&mut self, sect: &'ast Section) {
    walk_intf_section(self, sect);
  }

  fn visit_impl_sect(&mut self, sect: &'ast Section) {
    walk_impl_section(self, sect);
  }

  fn visit_imports(&mut self, imports: &'ast Vec<&str>) {
    // empty
  }

  fn visit_definition(&mut self, def_sect: &'ast Def) {
    walk_definition(self, def_sect);
  }

  fn visit_var_def(&mut self, var_def: &'ast VarDef) {
    walk_var_def(self, var_def);
  }

  fn visit_const_def(&mut self, const_def: &'ast ConstDef) {
    walk_const_def(self, const_def);
  }

  fn visit_type_def(&mut self, type_def: &'ast TypeDef) {
    walk_type_def(self, type_def);
  }

  fn visit_fn_definition(&mut self, fn_def: &'ast FnDef) {
    todo!()
  }

  fn visit_stmt(&mut self, stmt: &'ast Stmt) {
    walk_stmt(self, stmt);
  }

  fn visit_expr(&mut self, expr: &'ast Expr) {
    walk_expr(self, expr);
  }

  fn visit_type(&mut self, ty: &'ast Type) {
    walk_type(self, ty);
  }

  fn visit_lit(&mut self, lit: &'ast Lit) {
    // empty
  }

  fn visit_entity_id(&mut self, id: &'ast EntityId) {
    // empty
  }

  fn visit_const_val(&mut self, const_val: &'ast ConstVal) {
    walk_const_val(self, const_val);
  }

  fn visit_segment(&mut self, segment: &'ast Segment) {
    walk_segment(self, segment);
  }
}

pub fn walk_module<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, m: &'ast Module) {
  match *m {
    Module::Program(Program { name: _name, ref impl_sect, ref body }) => {
      v.visit_impl_sect(impl_sect);
      v.visit_stmt(body);
    }
    Module::Unit(Unit { name: _name, ref intf_section, ref impl_sect }) => {
      v.visit_intf_sect(intf_section);
      v.visit_impl_sect(impl_sect);
      // todo: v.visit_statement(init_stmt);
    }
  }
}

pub fn walk_section<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, s: &'ast Section) {
  let Section { imports, definitions } = &s;
  v.visit_imports(imports);
  for d in definitions {
    v.visit_definition(d);
  }
}

pub fn walk_intf_section<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, s: &'ast Section) {
  walk_section(v, s);
}

pub fn walk_impl_section<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, s: &'ast Section) {
  walk_section(v, s);
}

pub fn walk_definition<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, d: &'ast Def) {
  match *d {
    Def::Vars(ref list) => {
      for it in list {
        v.visit_var_def(it);
      }
    }
    Def::Consts(ref list) => {
      for it in list {
        v.visit_const_def(it);
      }
    }
    Def::Fn(ref func) => {
      for it in &func.args {
        v.visit_var_def(it);
      }
      v.visit_type(&func.result);
      if let Some(locals) = func.locals.as_ref() {
        v.visit_definition(locals);
      }
      if let Some(code) = func.code.as_ref() {
        v.visit_stmt(code);
      }
    }
    Def::Types(ref list) => {
      for it in list {
        v.visit_type_def(it);
      }
    }
  }
}

pub fn walk_stmt<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast Stmt) {
  match *node {
    Stmt::Block { ref list } => {
      for it in list {
        v.visit_stmt(it);
      }
    }
    Stmt::If { ref then, ref clause, ref or_else } => {
      v.visit_expr(clause);
      v.visit_stmt(then);
      v.visit_stmt(or_else);
    }
    Stmt::Case => {}
    Stmt::While { ref clause, ref body } => {
      v.visit_expr(clause);
      v.visit_stmt(body);
    }
    Stmt::Repeat { ref body, ref until } => {
      for it in body {
        v.visit_stmt(it);
      }
      v.visit_expr(until);
    }
    Stmt::For { ref init, ref target, ref body, var: _, inc: _ } => {
      v.visit_expr(init);
      v.visit_expr(target);
      v.visit_stmt(body);
    }
    Stmt::FnCall { ref args, ref id } => {
      v.visit_entity_id(id);
      for it in args {
        v.visit_expr(it);
      }
    }
    Stmt::Assign { .. } => {
      todo!();
    }
    Stmt::Break => {}
  }
}

pub fn walk_expr<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast Expr) {
  match *node {
    Expr::Literal { ref lit } => {
      v.visit_lit(lit);
    }
    Expr::Unary { op: _, ref expr } => {
      v.visit_expr(expr);
    }
    Expr::Binary { ref l, op: _, ref r } => {
      v.visit_expr(l);
      v.visit_expr(r);
    }
    Expr::VarAccess { ref ids } => {
      for (id, indexes) in ids {
        v.visit_entity_id(id);
        if let Some(indexes) = indexes.as_ref() {
          for idx in indexes {
            v.visit_expr(idx);
          }
        }
      }
    }
    Expr::FnCall { ref id, ref args } => {
      v.visit_entity_id(id);
      for it in args {
        v.visit_expr(it);
      }
    }
  }
}

pub fn walk_type<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast Type) {
  match *node {
    Type::Named(ref ty) => {
      v.visit_entity_id(ty);
    }
    Type::Range(ref ty) => {
      v.visit_const_val(&ty.start);
      v.visit_const_val(&ty.end);
    }
    Type::File(ref ty) => {
      v.visit_entity_id(&ty.of);
    }
    Type::Array(ref ty) => {
      for it in &ty.sizes {
        v.visit_segment(it);
      }
      v.visit_type(&ty.of);
    }
    Type::Record(ref ty) => {
      for it in &ty.fields {
        v.visit_var_def(it);
      }
    }
  }
}

pub fn walk_const_val<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast ConstVal) {
  match *node {
    ConstVal::Lit(ref lit) => {
      v.visit_lit(lit);
    }
    ConstVal::Id(ref id) => {
      v.visit_entity_id(id);
    }
  }
}

pub fn walk_segment<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast Segment) {
  // todo: looks like a copy of existing type, so Segment is a subset of Type
  match *node {
    Segment::Range(ref ty) => {
      v.visit_const_val(&ty.start);
      v.visit_const_val(&ty.end);
    }
    Segment::Id(ref id) => {
      v.visit_entity_id(id);
    }
  }
}

pub fn walk_var_def<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast VarDef) {
  v.visit_type(&node.of);
}

pub fn walk_const_def<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast ConstDef) {
  v.visit_lit(&node.1);
}

pub fn walk_type_def<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast TypeDef) {
  v.visit_type(&node.1);
}
