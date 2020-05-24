include("./src/MiniPascal.jl")
using .MiniPascal
const m = MiniPascal

if length(ARGS) != 1
  println("USAGE: julia compile.jl sourcefile")
  exit(1)
end

filename = ARGS[1]

if !isfile(filename)
  println("File $(filename) not found.")
  exit(1)
end

source = open(ARGS[1]) do file
  read(file, String)
end

source = try
  ascii(source)
catch e
  println("The source code contains non-ascii characters.")
  exit(1)
end

try
  io = IOBuffer()
  m.generate(source, io)
  code = String(take!(io))
  filename = "out.ll"
  file = open(filename, "w")
  println(file, code)
  close(file)
catch e
  showerror(stdout, e)
end

println()
exit(0)
