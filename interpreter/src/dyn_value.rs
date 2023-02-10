use std::collections::HashMap;

use parser::literals::Lit;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum DynValue<'a> {
  Int(i64),
  Bool(bool),
  Char(char),
  Float(f32),
  Str(String),
  Rec(String, HashMap<&'a str, DynValue<'a>>),
  Void,
}

impl<'a> From<&Lit<'a>> for DynValue<'a> {
  fn from(value: &Lit<'a>) -> Self {
    match *value {
      Lit::Int(i) | Lit::IntHex(i) => DynValue::Int(i),
      Lit::Char(ch) | Lit::CharHex(ch) => DynValue::Char(ch),
      Lit::Real(f) => DynValue::Float(f),
      Lit::Bool(b) => DynValue::Bool(b),
      Lit::Str(s) => DynValue::Str(s.to_owned()),
    }
  }
}

impl<'a> ToString for DynValue<'a> {
  fn to_string(&self) -> String {
      match self {
        DynValue::Int(x) => x.to_string(),
        DynValue::Bool(x) => x.to_string(),
        DynValue::Char(x) => x.to_string(),
        DynValue::Float(x) => x.to_string(),
        DynValue::Str(x) => x.to_string(),
        DynValue::Void => panic!("expected value got void"),
        _ => panic!("can't print values of this type"),
      }
  }
}

impl<'a> DynValue<'a> {
  pub fn type_name(&self) -> &str {
    use crate::DynValue::Void;
    use DynValue::*;
    match self {
      Int(_) => "integer",
      Bool(_) => "boolean",
      Char(_) => "char",
      Float(_) => "real",
      Str(_) => "string",
      Void => "void",
      Rec(name, _) => name.as_str(),
    }
  }

  pub fn is_assignable(&self, to: &DynValue) -> bool {
    use DynValue::*;
    match (to, self) {
      (Int(_), Int(_))
      | (Bool(_), Bool(_))
      | (Char(_), Char(_))
      | (Float(_), Float(_))
      | (Str(_), Str(_)) => true,
      _ => false,
    }
  }
}
