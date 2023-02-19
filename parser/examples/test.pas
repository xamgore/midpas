program test;procedure assert_eq(x, expected: integer);

var
  x, y: integer;

procedure incX(val: integer);
begin
  x := x + 1;
end;

function add5(val: integer): integer;
begin
  add5 := val + 5;
end;

begin
  assert_eq(x, 0);

  incX(x);
  assert_eq(x, 2);

  x := add5(x);
  writeln('body ', x);
end.
