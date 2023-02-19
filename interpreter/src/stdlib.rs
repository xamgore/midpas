use std::collections::HashMap;

use once_cell::unsync::Lazy;

use parser::EntityId;

use crate::dyn_value::DynValue;

type VarArgs<'a, 's> = &'s [DynValue<'a>];

pub type Func = for<'a, 's> fn(args: VarArgs<'a, 's>) -> DynValue<'a>;

type StdLib = HashMap<EntityId<'static>, Func>;

pub const STDLIB: Lazy<StdLib> = Lazy::new(|| {
  StdLib::from_iter([
    (EntityId::Internal { id: "assert" }, assert as Func),
    (EntityId::Internal { id: "assert_eq" }, assert_eq as Func),
    (EntityId::Internal { id: "write" }, write as Func),
    (EntityId::Internal { id: "writeln" }, writeln as Func),
  ])
});

fn write<'a>(args: VarArgs) -> DynValue<'a> {
  for val in args {
    print!("{}", val.to_string());
  }

  DynValue::Void
}

fn writeln<'a>(args: VarArgs) -> DynValue<'a> {
  write(args);
  println!();
  DynValue::Void
}

fn assert<'a>(args: VarArgs) -> DynValue<'a> {
  assert_eq!(args.len(), 2, "two arguments expected, got {}", args.len());
  match &args[0] {
    DynValue::Bool(val) => assert!(val, "expected true, got false"),
    val => panic!("expected bool value, got {:?}", val),
  }
  DynValue::Void
}

fn assert_eq<'a>(args: VarArgs) -> DynValue<'a> {
  assert_eq!(args.len(), 2, "two arguments expected, got {}", args.len());
  assert_eq!(args[0], args[1], "expected {:?}, got {:?}", args[1], args[0]);
  DynValue::Void
}
