include("Parser.jl")

using DataStructures

struct StaticAnalysisException <: Exception
  msg::String
end

default_int_value = -1
default_real_value = -1.0
default_bool_value = false
default_string_value = ""

struct SValue
  type::MPType
  value::Union{Int,Float64,Bool,String}
end

function SValue(tc::TokenClass)
  if tc == kw_int return SValue(MInt) end
  if tc == kw_real return SValue(MReal) end
  if tc == kw_bool return SValue(MBool) end
  if tc == kw_string return SValue(MString) end
end

function SValue(type::MPType)
  if type == MInt return SValue(type, default_int_value) end
  if type == MBool return SValue(type, default_bool_value) end
  if type == MString return SValue(type, default_string_value) end
end

(*)(left::SValue, right::SValue) = SValue(MInt, left.value * right.value)

function (+)(left::SValue, right::SValue)
  if left.type == MInt
    return SValue(MInt, left.value + right.value)
  elseif left.type == MString
    return SValue(MString, left.value * right.value)
  end
end

(-)(left::SValue, right::SValue) = SValue(MInt, left.value - right.value)
(÷)(left::SValue, right::SValue) = SValue(MInt, left.value ÷ right.value)
(==)(left::SValue, right::SValue) = SValue(MBool, left.value == right.value)
(<)(left::SValue, right::SValue) = SValue(left.type, left.value < right.value)
(&)(left::SValue, right::SValue) = SValue(left.type, left.value & right.value)
(!)(operand::SValue) = SValue(MBool, !operand.value)

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
  (kw_and, MBool, MBool) => MBool
)

abstract type SymbolTable end

mutable struct StackEntry
  var_name::String
  var_type::MPType
  val_identifier::String
  scope_level::Int
end

mutable struct StackST <: SymbolTable
  stack::Stack{StackEntry}
end

StackST() = StackST(Stack{StackEntry}())

function getentry(s::StackST, var_name)
  for entry in s
    entry.var_name == var_name && return entry
  end
end

function popscope(s::StackST)
  isempty(s) && return
  scope = first(s).scope_level
  while first(s).scope_level == scope
    pop!(s)
  end
end

function pushvar!(s::StackST, var_name::String, var_type::MPType, val_identifier::String, new_scope=false::Bool)
  old_scope = first(s).scope_level
  scope = new_scope ? old_scope + 1 : old_scope
  entry = StackEntry(var_name, var_type, val_identifier, scope)
  push!(s.stack, entry)
end

# The entry point for the static analyzer.
function static_analysis(AST::Program)
  st = StackST()
  # Process definitions
  # (not implemented yet)

  # Process main block
  static_analysis(AST.main.statements, st)
end

function static_analysis(b::Block, st::SymbolTable)
  for stmt in b.statements
    static_analysis(stmt, st)
  end
end

function static_analysis(p::Write, st::SymbolTable)
  static_analysis(p.arguments, st)
end

function static_analysis(a::Arguments, st::SymbolTable)
  for arg in a.arguments
    typecheck(arg, st)
  end
end

function typecheck(e::SimpleExpression, st::SymbolTable)
  firsttype = typecheck(e.terms[1][1], st)
  currenttype = firsttype
  if length(e.terms) > 1
    for (term, op) in e.terms[2:end] # In the first tuple, the op is the sign (plus/minus) of the term. 
      termtype = typecheck(term, st)
      newtype = binary_result_types[(op.class, currenttype, termtype)]
      if newtype == MError
        throw(StaticAnalysisException(
          "Type mismatch for binary operation \"$(op.lexeme)\" $(op.line)."
        ))
      end
      currenttype = newtype
    end
  end
  e.type = currenttype
end

function typecheck(r::RelationalExpression, st::SymbolTable)
  typecheck(r.left, st)
  typecheck(r.right, st)
  r.type = binary_result_types[(r.left.type, r.right.type, r.operation.class)]
end

function typecheck(l::Literal, st::SymbolTable)
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

function typecheck(t::Term, st::SymbolTable)
  factor_types = [tpl[1].type for tpl in t.factors]
  first_type = factor_types[1]
  for tpl in t.factors
    if tpl[1].type != first_type
      throw(StaticAnalysisException(
        "Type mismatch in operation $(tpl[2]) on line $(tpl[1].line)."
      ))
    end
  end
  t.type = first_type
  return t.type
end

