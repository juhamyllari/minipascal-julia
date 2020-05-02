module MiniPascal

import Base: *,+,-,÷,<,!,&,==,showerror

DEBUG = true

include("CodeGen.jl")

greet() = println("Hello World!")

end # module
