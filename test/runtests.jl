using Revise
using MiniPascal
using Test

const m = MiniPascal
const prog1 = """
program foo;

begin
  writeln(5 + 2);
end.
"""

const prog2 = """
program bar;

begin
  var a, b : int;
  a := 23;
  b := 42;
  writeln(a + b);
end.
"""

const prog3 = """
program defs;

function doubleit(x:int): int;
  begin
    return 2*x;
  end;

begin
  var a, b : int;
  a := 23;
  b := doubleit(a);
  writeln(b);
end.
"""

const prog4 = """
program foo;

begin
  writeln(101 % 3);
end.
"""

const prog5 = """
program foo;

begin
  writeln(true and false);
end.
"""

const prog6 = """
program foo;

begin
  writeln(not true or not false);
end.
"""

const prog7 = """
program foo;

begin
  var a, b : int;
  a := 7;
  b := 42;
  writeln(a + b);
end.
"""

const prog8 = """
program defs;

function doubleit(x:int): int;
  begin
    return 2*x;
  end;

begin
  var a, b : int;
  a := 23;
  b := doubleit(a);
  writeln(b);
end.
"""

const prog9 = """
program defs;

function double_and_return_orig(var x:int): int;
  begin
    var tmp: int;
    tmp := x;
    x := 2*x;
    return tmp;
  end;

begin
  var a, b: int;
  a := 23;
  b := double_and_return_orig(a);
  writeln(a);
  writeln(b);
end.
"""

const prog10 = """
program foo;

begin
  var x: int;
  x := 23;
  if x > 0
  then writeln(2*x)
  else writeln(x);
end.
"""

const prog11 = """
program foo;

begin
  var x, y : int;
  var s : string;
  y := 5;
  x := 0;
  while x < 10
  do begin
    if x <= y
      then s := " is less than or equal to "
      else s := " is greater than ";
    writeln(x, s, y);
    x := x+1;
  end;
end.
"""

const prog12 = """
program foo;

procedure printx();
begin
  writeln(x);
end;

begin
  var x: int;
  x := 42;
  printx();
  x := 23;
  printx();
end.
"""
@testset "MiniPascal.jl" begin
    # Write your own tests here.
    io = IOBuffer()
    m.generate(prog12, io)
    code = String(take!(io))
    # println(code)

    filename = "out.ll"
    file = open(filename, "w")
    println(file, code)
    close(file)
    run(`lli $(filename)`)

end
