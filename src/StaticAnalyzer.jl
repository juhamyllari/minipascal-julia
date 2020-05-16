include("Parser.jl")

using DataStructures

struct StaticAnalysisException <: Exception
  msg::String
end

default_int_value = -1
default_real_value = -1.0
default_bool_value = false
default_string_value = ""

initial_scope_level = 0

operator_to_function = Dict(
  times => *,
  plus => +,
  minus => -,
  divide => ÷,
  modulo => %,
  equals => ==,
  less_than => <
)

unary_result_types = Dict(
  (kw_not, MBool) => MBool
)

array_type_to_scalar_type = Dict(
  MIntRef => MInt,
  MRealRef => MReal,
  MBoolRef => MBool,
  MStringRef => MString
)

binary_result_types = DefaultDict(MError,
  (times, MInt, MInt) => MInt,
  (times, MReal, MReal) => MReal,
  (plus, MInt, MInt) => MInt,
  (plus, MReal, MReal) => MReal,
  (plus, MString, MString) => MString,
  (minus, MInt, MInt) => MInt,
  (minus, MReal, MReal) => MReal,
  (divide, MInt, MInt) => MInt,
  (divide, MReal, MReal) => MReal,
  (modulo, MInt, MInt) => MInt,
  (kw_or, MBool, MBool) => MBool,
  (kw_and, MBool, MBool) => MBool,
  (equals, MInt, MInt) => MBool,
  (not_equal, MInt, MInt) => MBool,
  (less_than, MInt, MInt) => MBool,
  (less_than_or_equal, MInt, MInt) => MBool,
  (greater_than, MInt, MInt) => MBool,
  (greater_than_or_equal, MInt, MInt) => MBool,
  (kw_and, MBool, MBool) => MBool,
  (equals, MReal, MReal) => MBool,
  (not_equal, MReal, MReal) => MBool,
  (less_than, MReal, MReal) => MBool,
  (less_than_or_equal, MReal, MReal) => MBool,
  (greater_than, MReal, MReal) => MBool,
  (greater_than_or_equal, MReal, MReal) => MBool,
  (equals, MString, MString) => MBool,
  (not_equal, MString, MString) => MBool,
  (less_than, MString, MString) => MBool,
  (less_than_or_equal, MString, MString) => MBool,
  (greater_than, MString, MString) => MBool,
  (greater_than_or_equal, MString, MString) => MBool,
  (equals, MBool, MBool) => MBool,
  (not_equal, MBool, MBool) => MBool,
  (less_than, MBool, MBool) => MBool,
  (less_than_or_equal, MBool, MBool) => MBool,
  (greater_than, MBool, MBool) => MBool,
  (greater_than_or_equal, MBool, MBool) => MBool,
)

abstract type SymbolTable end

mutable struct StackEntry
  var_name::String
  var_type::MPType
  val_identifier::String
  scope_level::Int
end

mutable struct Argument
  name::String
  is_var::Bool
  type::MPType
  size::Int
end

mutable struct SubroutineEntry
  name::String
  arg_types::Vector{MPType}
  return_type::MPType
end

mutable struct StackST <: SymbolTable
  stack::Stack{StackEntry}
  scope::Int
  subroutines::Vector{SubroutineEntry}
  ret_val_exprs::Vector{Value}
  StackST() = new(Stack{StackEntry}(), initial_scope_level, Vector{SubroutineEntry}(), Vector{Value}())
end

function hasvariable(s::StackST, var_name)
  if DEBUG
    println("This is hasentry, looking for variable $(var_name). The following names are known:")
    for entry in s.stack
      println(entry)
    end
  end
  for entry in s.stack
    if entry.var_name == var_name
      return true
    end
  end
  return false
end

function getvariable(s::StackST, var_name)
  return getfirst(entry -> entry.var_name == var_name, s.stack)
end

function popscope!(s::StackST)
  isempty(s.stack) && return
  scope = first(s.stack).scope_level
  while !isempty(s.stack) && first(s.stack).scope_level == scope
    pop!(s.stack)
  end
end

function enterscope!(s::StackST)
  s.scope += 1  
end

function pushvar!(st::StackST, var_name::String, var_type::MPType, val_identifier::String)
  entry = StackEntry(var_name, var_type, val_identifier, st.scope)
  push!(st.stack, entry)
end

function pushsubroutine!(st::StackST, name::String, arg_types::Vector{MPType}, return_type::MPType)
  entry = SubroutineEntry(name, arg_types, return_type)
  push!(st.subroutines, entry)
end

function hassubroutine(st::StackST, name::String)
  return any(entry->entry.name == name, st.subroutines)
end

function getsubroutine(st::StackST, name::String)
  index = findfirst(sr->sr.name == name, st.subroutines)
  println("this is getsr, index is $index")
  println("this is getsr, st contains $(st.subroutines)")
  return st.subroutines[index]
end

# The entry point for the static analyzer.
function static_analysis(AST::Program)
  st = StackST()
  
  # Process definitions
  static_analysis(AST.definitions, st)
  
  # Process main block
  static_analysis(AST.main, st)
end

function static_analysis(d::Definitions, st::SymbolTable)
  for definition in d.defs
    static_analysis(definition, st)
  end
end

function static_analysis(r::Return, st::SymbolTable)
  typecheck(r.value, st)
  r.type = MPassed
end

function static_analysis(s::Subroutine, st::SymbolTable)
  enterscope!(st)
  static_analysis(s.params, st)
  static_analysis(s.ret_type, st)
  static_analysis(s.body, st)
  popscope!(st)
  nominal_arg_types = [par.var_type for par in s.params.params]
  is_reftype = [(par.is_var_par || par.is_array) for par in s.params.params]
  println(is_reftype)
  println(nominal_arg_types)
  println(s.params.params)
  actual_types =
    [(is_ref ? scalar_to_ref_type[type] : type) for (type, is_ref) in zip(nominal_arg_types, is_reftype)]
  pushsubroutine!(st, s.name.lexeme, actual_types, s.ret_type.var_type)
  s.type = MPassed
end

function static_analysis(c::CallStatement, st::SymbolTable)
  static_analysis(c.arguments, st)
  subroutine_entry::SubroutineEntry = getsubroutine(st, c.identifier.lexeme)
  c.type = MPassed
end

function static_analysis(p::Parameters, st::SymbolTable)
  for parameter in p.params
    static_analysis(parameter, st)
  end
  p.type = MPassed
end

function static_analysis(p::Parameter, st::SymbolTable)
  typecheck(p.size, st)
  if p.size.type != MInt
    throw(StaticAnalysisException(
      "Array size must have integer value, got $(p.size.type) (line $(p.line))."
    ))
  end
  pushvar!(st, p.name, p.var_type, "")
  p.type = MPassed
end

function static_analysis(b::Block, st::SymbolTable)
  DEBUG && println("This is static analysis, analysing Block")
  enterscope!(st)
  for stmt in b.statements
    static_analysis(stmt, st)
  end
  popscope!(st)
  b.type = MPassed
end

function static_analysis(d::Declaration, st::SymbolTable)
  DEBUG && println("This is static analysis, analysing Declaration")
  var_type = d.var_type.var_type
  for name in map(token->token.lexeme, d.names)
    if hasvariable(st, name)
      throw(StaticAnalysisException(
        "Cannot redeclare variable $(name) (line $(d.line))."
      ))
    end
    pushvar!(st, name, var_type, "")
  end
  d.type = MPassed
end

function static_analysis(c::VarType, st::SymbolTable)
  typecheck(c.size, st)
  c.type = MPassed
end

function static_analysis(a::Assignment, st::SymbolTable)
  DEBUG && println("This is static analysis, analysing Assignment")
  var_name = a.variable.lexeme
  if !hasvariable(st, var_name)
    throw(StaticAnalysisException(
      "Cannot assign to undeclared variable '$(var_name)' (line $(a.line))."
    ))
  end
  value_type = typecheck(a.value, st)
  entry = getvariable(st, var_name)
  var_type = entry.var_type
  if value_type != var_type
    throw(StaticAnalysisException(
      "Cannot assign a value of type $(value_type) to variable '$(var_name)' of type $(var_type) (line $(a.line))."
    ))
  end
  a.type = MPassed
end

function static_analysis(i::IfThenElse, st::SymbolTable)
  cond_type = typecheck(i.condition, st)
  if cond_type != MBool
    throw(StaticAnalysisException(
    "If statement condition must be boolean, got type $cond_type (line $(i.line))."))
  end
  static_analysis(i.then_stmt, st)
  static_analysis(i.else_stmt, st)
  i.type = MPassed
end

function static_analysis(i::IfThen, st::SymbolTable)
  cond_type = typecheck(i.condition, st)
  if cond_type != MBool
    throw(StaticAnalysisException(
    "If statement condition must be boolean, got type $cond_type (line $(i.line))."))
  end
  static_analysis(i.then_stmt, st)
  i.type = MPassed
end

function static_analysis(w::While, st::SymbolTable)
  cond_type = typecheck(w.condition, st)
  if cond_type != MBool
    throw(StaticAnalysisException(
    "While statement condition must be boolean, got type $cond_type (line $(w.line))."))
  end
  static_analysis(w.do_stmt, st)
  w.type = MPassed
end

function static_analysis(p::Write, st::SymbolTable)
  DEBUG && println("This is static analysis, analysing Write")
  static_analysis(p.arguments, st)
  p.type = MPassed
end

function static_analysis(a::Vector{Value}, st::SymbolTable)
  DEBUG && println("This is static analysis, analysing arguments")
  for arg in a
    typecheck(arg, st)
  end
end

function typecheck(e::SimpleExpression, st::SymbolTable)
  DEBUG && println("This is static analysis, analysing SimpleExpression")
  firsttype = typecheck(e.terms[1][1], st)
  currenttype = firsttype
  if length(e.terms) > 1
    for (term, op) in e.terms[2:end] # In the first tuple, the op is the sign (plus/minus) of the term. 
      termtype = typecheck(term, st)
      newtype = binary_result_types[(op.class, currenttype, termtype)]
      if newtype == MError
        throw(StaticAnalysisException(
          "Type mismatch ($(currenttype) and $(termtype))for binary operation \"$(op.lexeme)\" $(op.line)."
        ))
      end
      currenttype = newtype
    end
  end
  e.type = currenttype
end

function typecheck(i::ImmediateInt, st)
  i.type = MInt
end

function typecheck(i::ImmediateReal, st)
  i.type = MReal
end

function typecheck(i::ImmediateString, st)
  i.type = MString
end

function typecheck(i::ImmediateBool, st)
  i.type = MBool
end

function typecheck(r::RelationalExpression, st::SymbolTable)
  DEBUG && println("This is static analysis, analysing RelationalExpression")
  typecheck(r.left, st)
  typecheck(r.right, st)
  result_type = binary_result_types[(r.operation.class, r.left.type, r.right.type)]
  if result_type == MError
    throw(StaticAnalysisException(
      "Type mismatch ($(r.left.type) and $(r.right.type)) in relational operation on line $(r.line)."
    ))
  end
  r.type = result_type
end

function typecheck(v::VarAsPar, st::SymbolTable)
  if !hasvariable(st, v.name)
    throw(StaticAnalysisException(
      "Variable name '$(v.name)' given as var argument is not defined (line $(v.line))."
    ))
  end
  variable::StackEntry = getvariable(st, v.name)
  v.type = scalar_to_ref_type[variable.var_type]
end

function typecheck(l::LiteralFactor, st::SymbolTable)
  DEBUG && println("This is static analysis, analysing LiteralFactor")
  class = l.token.class
  if class == int_literal
    l.type = MInt
  end
  if class == real_literal
    l.type = MReal
  end
  if class ∈ [kw_true, kw_false] 
    l.type = MBool
  end
  if class == string_literal
    l.type = MString
  end
  return l.type
end

function typecheck(v::VariableFactor, st::SymbolTable)
  DEBUG && println("This is static analysis, analysing VariableFactor")
  var_name = v.identifier.lexeme
  if !hasvariable(st, var_name)
    throw(StaticAnalysisException(
      "Variable $(var_name) is not defined (line $(v.line))."
    ))
  end
  v.type = getvariable(st, var_name).var_type
end

function typecheck(a::ArrayAccessFactor, st::SymbolTable)
  DEBUG && println("This is static analysis, analysing ArrayAccessFactor")
  index_type = typecheck(a.index, st)
  if index_type != MInt
    throw(StaticAnalysisException(
      "Array index should be an integer, got type $(index_type) (line $(a.line))."
    ))
  end
  var_name = a.identifier.lexeme
  if !hasvariable(st, var_name)
    throw(StaticAnalysisException(
      "Variable $(var_name) is not defined (line $(a.line))."
    ))
  end
  a.type = array_type_to_scalar_type[getvariable(st, var_name).var_type]
end

function typecheck(c::CallFactor, st::SymbolTable)
  static_analysis(c.arguments, st)
  subroutine_entry::SubroutineEntry = getsubroutine(st, c.identifier.lexeme)
  ret_type = subroutine_entry.return_type
  if ret_type == MNothing
    name = subroutine_entry.name
    throw(StaticAnalysisException(
      "A call to subroutine '$name' cannot be used as a value as it has no return type (line $(c.line))."
    ))
  end
  c.type = ret_type
end

function typecheck(p::ParenFactor, st::SymbolTable)
  p.type = typecheck(p.expression, st)
end

function typecheck(n::NotFactor, st::SymbolTable)
  arg_type = typecheck(n.argument, st)
  if arg_type != MBool
    throw(StaticAnalysisException(
      "The \"not\" operation requires a boolean argument, got type $(arg_type) on line $(n.line)."
    ))
  end
  n.type = MBool
end

function typecheck(t::Term, st::SymbolTable)
  factor_types = [typecheck(tpl[1], st) for tpl in t.factors]
  first_type = factor_types[1]
  for tpl in t.factors
    if tpl[1].type != first_type
      throw(StaticAnalysisException(
        "Type mismatch in operation $(tpl[2].class) on line $(tpl[1].line)."
      ))
    end
  end
  t.type = first_type
  return t.type
end

function typecheck(s::SizeFactor, st::SymbolTable)
  array_type = typecheck(s.array, st)
  if array_type ∉ [MIntRef, MRealRef, MBoolRef, MStringRef]
    throw(StaticAnalysisException(
      "Array size is not defined for values of type $(array_type) (on line $(s.line))."))
  end
  s.type = MInt
end
