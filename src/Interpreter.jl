include("StaticAnalyzer.jl")

struct ExecutionException <: Exception
  msg::String
  line::Int
end

Base.showerror(io::IO, e::LexicalException) = print(io,
  "A lexical error occurred: $(e.msg)\n")
Base.showerror(io::IO, e::SyntaxException) = print(io,
  "A syntax error occurred: $(e.msg)\n")
Base.showerror(io::IO, e::StaticAnalysisException) = print(io,
  "Static analysis produced an error: $(e.msg)\n")
Base.showerror(io::IO, e::ExecutionException) = print(io,
  "Static analysis produced an error on line $(e.line): $(e.msg)\n")

function run(program::String)
  AST = parseInput(scanInput(program))
  staticAnalysis(AST)
  executeProgram(AST)
end

function evaluate(l::Literal)
  if l.token.class == int_literal
    value = parse(Int, l.token.lexeme)
    return SValue(MInt, value)
  elseif l.token.class == string_literal
    return SValue(MString, l.token.lexeme)
  elseif l.token.class == kw_true
    return SValue(MBool, true)
  elseif l.token.class == kw_false
    return SValue(MBool, false)
  end
end

evaluate(l::Literal, st) = evaluate(l)

function evaluate(v::Var, st::SymbolTable)
  return getValue(st, v.variable.lexeme)
end
    
function evaluate(node::UnaryOperation, st::SymbolTable)
  operation = operator_to_function[node.operator.class]
  return operation(evaluate(node.operand, st))
end

function evaluate(node::BinaryOperation, st::SymbolTable)
  operation = operator_to_function[node.operator.class]
  left = evaluate(node.leftOperand, st)
  right = evaluate(node.rightOperand, st)
  if (operation == (÷) && right.value == 0)
    throw(ExecutionException("Divide by zero", node.line))
  end
  return operation(left, right)
end

function executeStatements(statements::Array{Statement,1}, st::SymbolTable)
  for s in statements
    executeStatement(s, st)
  end
end

executeStatements(s::Statements, st::SymbolTable) =
  executeStatements(s.statements, st)

executeStatements(s::Array{Statement,1}) =
  executeStatements(s, SymbolTable())

executeProgram(p::Statements) = executeStatements(p.statements)

function executeStatement(d::Declaration, st::SymbolTable)
  addOrUpdateSymbol(st, d.variable.lexeme, SValue(d.type.class))
end

# function executeStatement(d::DecAssignment, st::SymbolTable)
#   addOrUpdateSymbol(st, d.variable.lexeme, evaluate(d.value, st))
# end

function executeStatement(a::Assignment, st::SymbolTable)
  addOrUpdateSymbol(st, a.variable.lexeme, evaluate(a.value, st))
end

function executeStatement(f::For, st::SymbolTable)
  varName = f.variable.lexeme
  from = evaluate(f.from, st).value
  to = evaluate(f.to, st).value
  for i in from:to
    setIterationVariable(st, varName, i)
    executeStatements(f.body, st)
  end
  releaseIterationVariable(st, varName)
end

function executeStatement(p::Write, st::SymbolTable)
    print(evaluate(p.argument, st).value)
end

function executeStatement(r::Read, st::SymbolTable)
  varName = r.variable.lexeme
  type = getValue(st, varName).type
  println("\n<MiniPL-Julia is waiting for input>")
  rawInput = split(readline())[1]
  if type == MInt
    addOrUpdateSymbol(st, varName, SValue(MInt, parse(Int, rawInput)))
  elseif type == MString
    addOrUpdateSymbol(st, varName, SValue(MString, string(rawInput)))
  elseif type == MBool
    addOrUpdateSymbol(st, varName, SValue(MBool, parse(Bool, rawInput)))
  end
end

function executeStatement(a::Assert, st::SymbolTable)
  if !(evaluate(a.argument, st).value)
    println("Assertion on line $(a.line) failed.")
  end
end
