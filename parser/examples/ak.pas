program test;procedure assert_eq(x, y: integer);

function A(m, n: integer): integer;
begin
  if m = 0 then A := n + 1
  else begin if n = 0 then A := A(m - 1, 1)
  else A := A(m - 1, A(m, n - 1)) end
end;

begin
  assert_eq(A(3, 10), 8189);
  writeln('Correct!');
end;

