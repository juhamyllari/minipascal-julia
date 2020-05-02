using Revise
using MiniPascal
using Test

const m = MiniPascal
const prog1 = """
program foo;

begin
  writeln 5 + 2;
end.
"""

@testset "MiniPascal.jl" begin
    # Write your own tests here.
    # io = IOBuffer()
    # m.generate(prog5, io)
    # code = String(take!(io))
    # println(code)

    # file = open("out.ll", "w")
    # println(file, code)
    # close(file)
    parsed = m.parseInput(prog1)
    print(parsed)
end
