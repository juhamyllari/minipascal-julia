using Revise
using MiniPascal
using Test

const m = MiniPascal

const progs = ["""
program foo;

begin
  writeln(5 + 2);
end.
""",

"""
program prog2;

begin
  var a, b : int;
  a := 23;
  b := 42;
  writeln(a + b);
end.
""",

"""
program prog3;

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
""",

"""
program prog4;

begin
  writeln(101 % 3);
end.
""",

"""
program prog5;

begin
  writeln(true and false);
end.
""",

"""
program prog6;

begin
  writeln(not true or not false);
end.
""",

"""
program prog7;

begin
  var a, b : int;
  a := 7;
  b := 42;
  writeln(a + b);
end.
""",

"""
program prog8;

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
""",

"""
program prog9;

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
""",

"""
program prog10;

begin
  var x: int;
  x := 23;
  if x > 0
  then writeln(2*x)
  else writeln(x);
end.
""",

"""
program prog11;

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
""",

"""
program prog12;

procedure printx();
begin
  writeln(x);
end;

procedure increment(var z:int);
begin
  z := z+1;
end;

begin
  var x: int;
  x := 42;
  printx();
  begin
    var x:int;
    x := 22;
    increment(x);
    printx();
  end;
  printx();
end.
""",

"""
program prog13;

begin
  var x: real;
  x := 23.0;
  if (x > 0.0) and true
  then writeln(2.0*x)
  else writeln(x);
end.
""",

"""
program prog14;

begin
  var s,x,y,z: string;
  x := "Hello";
  y := "hello";
  z := ", world!";
  if x < y
  then s:=x+z
  else s:=y+z;
  writeln(s)
end.
""",

"""
program prog15;

function fact(x:int):int;
begin
  if (x = 0) or (x = 1)
    then return 1;
  return x * fact(x-1);
end;

begin
  var x: int;
  x := 5
  writeln(fact(x));
end.
""",

"""
program prog16;

begin
  writeln(-2 + 44);
end.
""",

"""
program prog17;
function myadd(x:int, y:int):int;
  begin
    return x+y;
  end;

begin
  var a,b:int
  var c:boolean
  writeln(myadd(22,20));
end.
""",

"""
program prog18;
function myadddoubleds(x:int, y:int):int;
  begin
    return mydouble(x)+mydouble(y);
  end;

function mydouble(a:real):real;
  begin
    return 42.0 {* My favourite double *};
  end;

function mydouble(a:int):int;
  begin
    return a*2;
  end;

begin
  var a,b:int
  var c:boolean
  writeln(myadddoubleds(22,20));
end.
""",
]

# Write your own tests here.
function run_prog(i::Int)
  io = IOBuffer()
  m.generate(progs[i], io)
  code = String(take!(io))
  filename = "out.ll"
  file = open(filename, "w")
  println(file, code)
  close(file)
  run(`lli $(filename)`)
end

# @testset "MiniPascal.jl" begin
  run_prog(1)
# end

