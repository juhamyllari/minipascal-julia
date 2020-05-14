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


@testset "MiniPascal.jl" begin
    # Write your own tests here.
    io = IOBuffer()
    m.generate(prog8, io)
    code = String(take!(io))
    println(code)

    filename = "out2.ll"
    file = open(filename, "w")
    println(file, code)
    close(file)
    run(`lli $(filename)`)


    # parsed = m.parseInput(prog3)
    # print(parsed)
    # m.static_analysis(parsed)
    # print(parsed)
end
