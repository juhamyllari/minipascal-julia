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

unary_result_types = Dict{Tuple{TokenClass,MPType},MPType}(
  (kw_not, MBool) => MBool
)

array_type_to_scalar_type = Dict{MPType,MPType}(
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

mutable struct Argument
  name::String
  is_var::Bool
  type::MPType
  size::Int
end

mutable struct SubroutineEntry
  name::String
  param_names::Vector{String}
  param_types::Vector{MPType}
  return_type::MPType
  node::Union{Subroutine,Nothing}
end

mutable struct AnalysisContext
  scope_stack::Stack{SymTableEntry}
  scope::Int
  subroutines::Vector{SubroutineEntry}
  in_call::Stack{Call}
  unique_calls::Vector{Call}
  var_id_counter::Int
end
AnalysisContext() = AnalysisContext(
      Stack{SymTableEntry}(),
      initial_scope_level,
      Vector{SubroutineEntry}(),
      Stack{Call}(),
      Vector{Call}(),
      0
      )

function create_var_id(ac::AnalysisContext, var_type::MPType, var_name::String)
  return "%_$(var_type)_$(var_name)_$(ac.var_id_counter)"
end

function hasvariable(s::AnalysisContext, var_name::String, inner_scope_only::Bool=false)
  if DEBUG
    println("This is hasentry, looking for variable $(var_name). The following names are known:")
    foreach(println, s.scope_stack)
  end
  if isempty(s.scope_stack) return false end
  if inner_scope_only
    scope = first(s.scope_stack).scope_level
    return any(entry->entry.scope_level == scope && entry.var_name == var_name, s.scope_stack)
  end
  return any(entry->entry.var_name == var_name, s.scope_stack)
end

function getvariable(s::AnalysisContext, var_name)
  return getfirst(entry -> entry.var_name == var_name, s.scope_stack)
end

function popscope!(s::AnalysisContext)
  println("This is popscope!, scope is $(s.scope)")
  isempty(s.scope_stack) && return
  s.scope -= 1
  println("This is popscope!, new scope is $(s.scope)")
  while !isempty(s.scope_stack) && (first(s.scope_stack).scope_level > s.scope)
    println("This is popscope!, popping $(first(s.scope_stack))")
    pop!(s.scope_stack)
  end
end

function enterscope!(s::AnalysisContext)
  s.scope += 1  
end

function pushvar!(ac::AnalysisContext, var_name::String, var_type::MPType, val_identifier::String)
  entry = SymTableEntry(var_name, var_type, val_identifier, ac.scope)
  push!(ac.scope_stack, entry)
end

function pushsubroutine!(ac::AnalysisContext,
    name::String,
    param_names::Vector{String},
    param_types::Vector{MPType},
    return_type::MPType,
    node::Union{Subroutine,Nothing}=nothing)
  entry = SubroutineEntry(name, param_names, param_types, return_type, node)
  push!(ac.subroutines, entry)
end

function hassubroutine(ac::AnalysisContext, name::String)
  return any(entry->entry.name == name, ac.subroutines)
end

function getsubroutine(srs::Vector{SubroutineEntry}, name::String)
  index = findfirst(sr->sr.name == name, srs)
  return srs[index]
end

function getsubroutine(ac::AnalysisContext, name::String)
  getsubroutine(ac.subroutines, name)
end

# The entry point for the static analyzer.
function static_analysis(AST::Program, ac::AnalysisContext)
  
  # Process definitions
  static_analysis(AST.definitions, ac)
  
  # Process main block
  static_analysis(AST.main, ac)
end

static_analysis(AST::Program) = static_analysis(AST, AnalysisContext())

function static_analysis(d::Definitions, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing definitions")
  for subroutine in d.defs
    get_signature(subroutine, ac)
    # static_analysis(definition, ac)
  end
end

function get_signature(s::Subroutine, ac::AnalysisContext)
  foreach(param::Parameter->static_analysis(param, ac), s.params)
  true_types = Vector{MPType}([parameter_to_mptype(param) for param in s.params])
  param_names = Vector{String}([param.name for param in s.params])
  static_analysis(s.ret_type, ac)
  if !isa(s.ret_type.size, ImmediateInt)
    throw(StaticAnalysisException(
      "Reference types (strings and arrays) are not allowed as function return types (line $(s.line))."
    ))
  end
  pushsubroutine!(ac,
    s.name,
    param_names,
    true_types,
    s.ret_type.scalar_type,
    s
  )
end

function parameter_to_mptype(p::Parameter)
  if p.is_array && p.is_var_par
    throw(StaticAnalysisException(
      "Array type cannot be used as var parameter (line $(p.line))."
    ))
  end
  if p.is_var_par
    return scalar_to_ref_type[p.scalar_type]
  end
  if p.is_array
    return scalar_to_array_type[p.scalar_type]
  end
  return p.scalar_type
end

function static_analysis(s::Subroutine, ac::AnalysisContext)
  static_analysis(s.body, ac)
  s.type = MPassed
end

function static_analysis(c::CallStatement, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing CallStatement")
  analyze_call(c, ac)
  c.type = MPassed
end

function analyze_call(c::Call, ac::AnalysisContext)
  DEBUG && println("This is analyze_call, call is to $(c.identifier)")
  foreach(arg->typecheck(arg, ac), c.arguments)
  subroutine_entry::SubroutineEntry = getsubroutine(ac, c.identifier)
  c.subroutine = subroutine_entry.node
  compare_with_signature(subroutine_entry, c.arguments, c.line)
  enterscope!(ac)
  push!(ac.in_call, c)
  for (param_name, arg) in zip(subroutine_entry.param_names, c.arguments)
    pushvar!(ac, param_name, arg.type, "")
  end
  fingerprint = symbol_fingerprint(ac)
  c.fingerprint = fingerprint
  if fingerprint ∉ c.subroutine.fingerprints
    println("fingerprint $fingerprint not found")
    static_analysis(subroutine_entry.node, ac)
    push!(c.subroutine.fingerprints, fingerprint)
    push!(c.subroutine.implicit_param_lists, c.implicit_params)
  end
  pop!(ac.in_call)
  popscope!(ac)
  if !isempty(ac.in_call) 
    for param in subroutine_entry.node.implicit_params
      entry::SymTableEntry = getvariable(ac, param)
      if entry.scope_level < ac.scope
        push!(first(ac.in_call).implicit_params, entry)
      end
    end
  end
  add_call_if_unique(c, ac)
end

function add_call_if_unique(c::Call, ac::AnalysisContext)
  if !any(call->call.identifier == c.identifier && call.implicit_params == c.implicit_params, ac.unique_calls)
    push!(ac.unique_calls, c)
  end
end

function symbol_fingerprint(ac::AnalysisContext)
  names_types = ["$(e.var_name)/$(e.var_type)/$(e.val_identifier)" for e in ac.scope_stack]
  return join(names_types, "+")
end

function compare_with_signature(sr::SubroutineEntry, args::Vector{Value}, line::Int)
  if length(sr.param_types) != length(args)
    throw(StaticAnalysisException(
      "Subroutine call has $(length(args)) arguments, expected $(length(sr.param_types)) (line $line)."
    ))
  end
  for (param_type, arg_type) in zip(sr.param_types, map(arg->arg.type, args))
    if arg_type != param_type
      throw(StaticAnalysisException(
        "Expected argument of type $param_type, got $arg_type (line $line)."
    ))
    end
  end
end

function static_analysis(r::Return, ac::AnalysisContext)
  current_subroutine::SubroutineEntry = first(ac.in_call)
  typecheck(r.value, ac)
  if r.value.type != current_subroutine.return_type
    throw(StaticAnalysisException(
      "Expected return type to be $(current_subroutine.return_type), got $(r.value.type) (line $(r.line))."
    ))
  end
  r.type = MPassed
end

function static_analysis(p::Parameter, ac::AnalysisContext)
  typecheck(p.size, ac)
  if p.size.type != MInt
    throw(StaticAnalysisException(
      "Array size must have integer value, got $(p.size.type) (line $(p.line))."
    ))
  end
  pushvar!(ac, p.name, p.scalar_type, "")
  p.type = MPassed
end

function static_analysis(b::Block, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing Block")
  if !b.is_subroutine_block enterscope!(ac) end
  for stmt in b.statements
    static_analysis(stmt, ac)
  end
  if !b.is_subroutine_block popscope!(ac) end
  b.type = MPassed
end

function static_analysis(d::Declaration, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing Declaration")
  var_type = d.var_type.scalar_type
  for name in map(token->token.lexeme, d.names)
    if hasvariable(ac, name)
      throw(StaticAnalysisException(
        "Cannot redeclare variable $(name) (line $(d.line))."
      ))
    end
    id = create_var_id(ac, var_type, name)
    push!(d.unique_ids, id)
    pushvar!(ac, name, var_type, id)
  end
  d.type = MPassed
end

function static_analysis(c::TypeOfVarOrValue, ac::AnalysisContext)
  typecheck(c.size, ac)
  c.type = MPassed
end

function static_analysis(a::Assignment, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing Assignment")
  var_name = a.variable_name
  if !hasvariable(ac, var_name)
    throw(StaticAnalysisException(
      "Cannot assign to undeclared variable '$(var_name)' (line $(a.line))."
    ))
  end
  index_type = typecheck(a.array_index, ac)
  if index_type != MInt
    throw(StaticAnalysisException(
      "Array index must be an integer, got $(index_type) (line $(a.line))."
    ))
  end
  value_type = typecheck(a.value, ac)
  entry::SymTableEntry = getvariable(ac, var_name)
  var_type = entry.var_type
  if value_type != var_type
    throw(StaticAnalysisException(
      "Cannot assign a value of type $(value_type) to variable '$(var_name)' of type $(var_type) (line $(a.line))."
    ))
  end
  a.var_unique_id = entry.val_identifier
  a.type = MPassed
end

function static_analysis(i::IfThenElse, ac::AnalysisContext)
  cond_type = typecheck(i.condition, ac)
  if cond_type != MBool
    throw(StaticAnalysisException(
    "If statement condition must be boolean, got type $cond_type (line $(i.line))."))
  end
  static_analysis(i.then_stmt, ac)
  static_analysis(i.else_stmt, ac)
  i.type = MPassed
end

function static_analysis(i::IfThen, ac::AnalysisContext)
  cond_type = typecheck(i.condition, ac)
  if cond_type != MBool
    throw(StaticAnalysisException(
    "If statement condition must be boolean, got type $cond_type (line $(i.line))."))
  end
  static_analysis(i.then_stmt, ac)
  i.type = MPassed
end

function static_analysis(w::While, ac::AnalysisContext)
  cond_type = typecheck(w.condition, ac)
  if cond_type != MBool
    throw(StaticAnalysisException(
    "While statement condition must be boolean, got type $cond_type (line $(w.line))."))
  end
  static_analysis(w.do_stmt, ac)
  w.type = MPassed
end

function static_analysis(p::Write, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing Write")
  static_analysis(p.arguments, ac)
  p.type = MPassed
end

function static_analysis(a::Vector{Value}, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing arguments")
  for arg in a
    typecheck(arg, ac)
  end
end

function typecheck(e::SimpleExpression, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing SimpleExpression")
  firsttype = typecheck(e.terms[1][1], ac)
  currenttype = firsttype
  if length(e.terms) > 1
    for (term, op) in e.terms[2:end] # In the first tuple, the op is the sign (plus/minus) of the term. 
      termtype = typecheck(term, ac)
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

function typecheck(i::ImmediateInt, ac)
  i.type = MInt
end

function typecheck(i::ImmediateReal, ac)
  i.type = MReal
end

function typecheck(i::ImmediateString, ac)
  i.type = MString
end

function typecheck(i::ImmediateBool, ac)
  i.type = MBool
end

function typecheck(r::RelationalExpression, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing RelationalExpression")
  typecheck(r.left, ac)
  typecheck(r.right, ac)
  result_type = binary_result_types[(r.operation.class, r.left.type, r.right.type)]
  if result_type == MError
    throw(StaticAnalysisException(
      "Type mismatch ($(r.left.type) and $(r.right.type)) in relational operation on line $(r.line)."
    ))
  end
  r.type = result_type
end

function typecheck(v::VarAsArgument, ac::AnalysisContext)
  if !hasvariable(ac, v.name)
    throw(StaticAnalysisException(
      "Variable name '$(v.name)' given as var argument is not defined (line $(v.line))."
    ))
  end
  variable::SymTableEntry = getvariable(ac, v.name)
  v.type = scalar_to_ref_type[variable.var_type]
end

function typecheck(l::LiteralFactor, ac::AnalysisContext)
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

function typecheck(v::VariableFactor, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing VariableFactor")
  DEBUG && println("in_call empty? $(isempty(ac.in_call))")
  var_name = v.identifier
  if !hasvariable(ac, var_name)
    throw(StaticAnalysisException(
      "Variable $(var_name) is not defined (line $(v.line))."
    ))
  end
  var_entry::SymTableEntry = getvariable(ac, var_name)
  if var_entry.scope_level < ac.scope && !isempty(ac.in_call)
    push!(first(ac.in_call).implicit_params, var_entry)
  end
  v.variable_entry = var_entry
  v.type = var_entry.var_type
end

function typecheck(a::ArrayAccessFactor, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing ArrayAccessFactor")
  index_type = typecheck(a.index, ac)
  if index_type != MInt
    throw(StaticAnalysisException(
      "Array index should be an integer, got type $(index_type) (line $(a.line))."
    ))
  end
  var_name = a.identifier.lexeme
  if !hasvariable(ac, var_name)
    throw(StaticAnalysisException(
      "Variable $(var_name) is not defined (line $(a.line))."
    ))
  end
  var_type = getvariable(ac, var_name).var_type
  if var_type ∉ [MIntRef, MRealRef, MBoolRef, MStringRef]
    throw(StaticAnalysisException(
      "Cannot index into non-array type ($var_type) variable '$var_name' (line $(a.line))."
    ))
  end
  a.type = array_type_to_scalar_type[var_type]
end

function typecheck(c::CallFactor, ac::AnalysisContext)
  DEBUG && println("This is static analysis, analysing CallFactor")
  analyze_call(c, ac)
  subroutine_entry::SubroutineEntry = getsubroutine(ac, c.identifier.lexeme)
  ret_type = subroutine_entry.return_type
  if ret_type == MNothing
    name = subroutine_entry.name
    throw(StaticAnalysisException(
      "A call to subroutine '$name' cannot be used as a value as it has no return type (line $(c.line))."
    ))
  end
  c.type = ret_type
end

function typecheck(p::ParenFactor, ac::AnalysisContext)
  p.type = typecheck(p.expression, ac)
end

function typecheck(n::NotFactor, ac::AnalysisContext)
  arg_type = typecheck(n.argument, ac)
  if arg_type != MBool
    throw(StaticAnalysisException(
      "The \"not\" operation requires a boolean argument, got type $(arg_type) on line $(n.line)."
    ))
  end
  n.type = MBool
end

function typecheck(t::Term, ac::AnalysisContext)
  factor_types = [typecheck(tpl[1], ac) for tpl in t.factors]
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

function typecheck(s::SizeFactor, ac::AnalysisContext)
  array_type = typecheck(s.array, ac)
  if array_type ∉ [MIntRef, MRealRef, MBoolRef, MStringRef]
    throw(StaticAnalysisException(
      "Array size is not defined for values of type $(array_type) (on line $(s.line))."))
  end
  s.type = MInt
end
