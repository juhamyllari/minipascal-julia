include("Scanner.jl")

struct SyntaxException <: Exception
  msg::String
end

@enum MPType begin
  MInt
  MReal
  MBool
  MString
  MIntArray
  MRealArray
  MBoolArray
  MStringArray
  MIntRef
  MRealRef
  MBoolRef
  MStringRef
  MNothing
  MError
  MPassed
  MUndefined
end

const ref_types = Vector{MPType}([
  MIntRef,
  MRealRef,
  MBoolRef,
  MStringRef
])

const array_types = Vector{MPType}([
  MIntArray,
  MRealArray,
  MBoolArray,
  MStringArray
])

const scalar_types = Vector{MPType}([
  MInt,
  MReal,
  MBool,
  MString
])

token_class_to_scalar_type = Dict{TokenClass,MPType}(
  kw_int => MInt,
  kw_real => MReal,
  kw_boolean => MBool,
  kw_string => MString
)

token_class_to_ref_type = Dict{TokenClass,MPType}(
  kw_int => MIntRef,
  kw_real => MRealRef,
  kw_boolean => MBoolRef,
  kw_string => MStringRef
)

scalar_to_ref_type = Dict{MPType,MPType}(
  MInt => MIntRef,
  MReal => MRealRef,
  MBool => MBoolRef,
  MString => MStringRef,
  MIntRef => MIntRef,
  MRealRef => MRealRef,
  MBoolRef => MBoolRef,
  MStringRef => MStringRef,
  MIntArray => MIntArray,
  MRealArray => MRealArray,
  MBoolArray => MBoolArray,
  MStringArray => MStringArray
)

scalar_to_array_type = Dict{MPType,MPType}(
  MInt => MIntArray,
  MReal => MRealArray,
  MBool => MBoolArray,
  MString => MStringArray
)

mutable struct ImplicitParameter
  name::String
  type::MPType
  unique_id::String    
end

mutable struct SymTableEntry
  var_name::String
  var_type::MPType
  val_identifier::String
  scope_level::Int
  is_implicit::Bool
end
SymTableEntry(var_name::String, var_type::MPType, val_identifier::String, scope_level::Int) =
  SymTableEntry(var_name::String, var_type::MPType, val_identifier::String, scope_level::Int, false)

abstract type Node end
# Pseudonodes are nodelike structs that assist in parsing
# but are not included in the abstract syntax tree (AST).
abstract type Pseudonode <: Node end
abstract type Statement <: Node end
abstract type Value <: Node end
abstract type Factor <: Value end
abstract type If <: Statement end

mutable struct BinaryOperation <: Value
  op::TokenClass
  left::Value
  right::Value
  line::Int
  type::MPType
end
BinaryOperation(op, left, right, line) = BinaryOperation(op, left, right, line, MUndefined)

mutable struct UnaryOperation <: Value
  op::TokenClass
  operand::Value
  line::Int
  type::MPType
end
UnaryOperation(op, operand, line) = UnaryOperation(op, operand, line, MUndefined)

mutable struct ImmediateInt <: Value
  value::Int
  type::MPType
end
ImmediateInt(value::Int) = ImmediateInt(value, MUndefined)

mutable struct ImmediateReal <: Value
  value::Float64
  type::MPType
end
ImmediateReal(value::Float64) = ImmediateReal(value, MUndefined)

mutable struct ImmediateString <: Value
  value::String
  type::MPType
end
ImmediateString(value::String) = ImmediateString(value, MUndefined)

mutable struct ImmediateBool <: Value
  value::Bool
  type::MPType
end
ImmediateBool(value::Bool) = ImmediateBool(value, MUndefined)

mutable struct Block <: Statement
  statements::Vector{Statement}
  is_subroutine_block::Bool
  subroutine::Union{Node,Nothing}
  line::Int
  type::MPType
end
Block(statements::Vector{Statement}, line::Int) = Block(statements, false, nothing, line, MUndefined)

mutable struct TypeOfVarOrValue <: Node
  scalar_type::MPType
  true_type::MPType
  size::Value
  line::Int
  type::MPType
end
TypeOfVarOrValue(scalar_type::MPType, true_type::MPType, size::Value, line::Int) =
  TypeOfVarOrValue(scalar_type, true_type, size, line, MUndefined)

function is_array_type(t::TypeOfVarOrValue)
  return t.size isa ImmediateInt && t.size.value == 1
end

mutable struct Parameter <: Node
  name::String
  scalar_type::MPType
  is_var_par::Bool
  is_array::Bool
  size::Value       # Array size can only be determined at run time
  line::Int
  type::MPType
end
Parameter(name::String,
  scalar_type::MPType,
  is_var_par::Bool,
  is_array::Bool,
  size::Value,
  line::Int) =
  Parameter(name,
    scalar_type,
    is_var_par,
    is_array,
    size,
    line,
    MUndefined)

# Function or procedure.
mutable struct Subroutine <: Node
  name::String
  params::Vector{Parameter}
  ret_type::TypeOfVarOrValue
  body::Block
  calls_subroutines::Vector{String}
  llvm_function_name::String
  is_called::Bool   # No codegen for subroutines that are never called
  line::Int
  type::MPType
end
Subroutine(name, params, ret_type, body, line) =
  Subroutine(
    name,
    params,
    ret_type,
    body,
    Vector{String}(),
    "",
    false,
    line,
    MUndefined) 

mutable struct Definitions <: Node
  defs::Vector{Subroutine}
  line::Int
  type::MPType
  end
Definitions(defs, line) = Definitions(defs, line, MUndefined)

mutable struct Program <: Node
  definitions::Definitions
  main::Block
  line::Int
  type::MPType
  end
Program(definitions, main, line) = Program(definitions, main, line, MUndefined)

mutable struct CallStatement <: Statement
  identifier::String
  arguments::Vector{Value}
  implicit_params::Vector{SymTableEntry}
  subroutine::Union{Subroutine,Nothing}
  call_id::Int
  fingerprint::String
  scope_level::Int
  line::Int
  type::MPType
end
CallStatement(identifier, arguments, call_id, line) =
  CallStatement(identifier, arguments, Vector{SymTableEntry}(), nothing, call_id, "", 0, line, MUndefined)

mutable struct Declaration <: Statement
  var_type::TypeOfVarOrValue
  names::Vector{Token}
  unique_ids::Vector{String}
  line::Int
  type::MPType
end
Declaration(var_type, names, line) = Declaration(var_type, names, Vector{String}(), line, MUndefined)

mutable struct Variable <: Pseudonode
  identifier::String
  is_array_access::Bool
  array_index::Value
end

mutable struct Assignment <: Statement
  variable_name::String
  var_unique_id::String
  var_type::MPType
  is_array_access::Bool
  array_index::Value  # Array index. Can use ImmediateInt(-1) for non-array access assignments.
  value::Value
  line::Int
  type::MPType
end
Assignment(variable_name::String, is_array_access::Bool, index::Value, value::Value, line::Int) =
  Assignment(variable_name, "", MUndefined, is_array_access, index, value, line, MUndefined)

mutable struct IfThen <: If
  condition::Value
  then_stmt::Statement
  line::Int
  type::MPType
end
IfThen(condition, then_stmt, line) = IfThen(condition, then_stmt, line, MUndefined)

mutable struct IfThenElse <: If
  condition::Value
  then_stmt::Statement
  else_stmt::Statement
  line::Int
  type::MPType
end
IfThenElse(condition, then_stmt, else_stmt, line) = IfThenElse(condition, then_stmt, else_stmt, line, MUndefined)

mutable struct While <: Statement
  condition::Value
  do_stmt::Statement
  line::Int
  type::MPType
end
While(condition, do_stmt, line) = While(condition, do_stmt, line, MUndefined)

mutable struct Read <: Statement
  variable::Token
  line::Int
  type::MPType
end
Read(variable, line) = Read(variable, line, MUndefined)

mutable struct Write <: Statement
  arguments::Vector{Value}
  line::Int
  type::MPType
end
Write(arguments, line) = Write(arguments, line, MUndefined)

mutable struct Assert <: Statement
  argument::Value
  line::Int
  type::MPType
end
Assert(argument, line) = Assert(argument, line, MUndefined)

mutable struct Return <: Statement
  value::Value
  line::Int
  type::MPType
end
Return(value, line) = Return(value, line, MUndefined)

mutable struct SizeFactor <: Factor
  array::Factor
  line::Int
  type::MPType
  end
SizeFactor(array, line) = SizeFactor(array, line, MUndefined)

mutable struct NotFactor <: Factor
  argument::Factor
  line::Int
  type::MPType
end
NotFactor(argument, line) = NotFactor(argument, line, MUndefined)

mutable struct ParenFactor <: Factor
  expression::Value
  line::Int
  type::MPType
end
ParenFactor(expression, line) = ParenFactor(expression, line, MUndefined)

mutable struct LiteralFactor <: Factor
  token::Token
  unique_id::String  # Used to declare string literals
  line::Int
  type::MPType
end
LiteralFactor(token, index, line) = LiteralFactor(token, index, line, MUndefined)

mutable struct VariableFactor <: Factor
  identifier::String
  variable_entry::Union{SymTableEntry,Nothing}
  line::Int
  type::MPType
end
VariableFactor(identifier::String, line::Int) = VariableFactor(identifier, nothing, line, MUndefined)

mutable struct GetRef <: Value
  operand::VariableFactor
  line::Int
  type::MPType
end
GetRef(operand::VariableFactor, line::Int) = GetRef(operand, line, MUndefined)

mutable struct ArrayAccessFactor <: Factor
  identifier::String
  index::Expr
  line::Int
  type::MPType
end
ArrayAccessFactor(identifier, index, line) = ArrayAccessFactor(identifier, index, line, MUndefined)

mutable struct CallFactor <: Factor
  identifier::String
  arguments::Vector{Value}
  implicit_params::Vector{SymTableEntry}
  subroutine::Union{Subroutine,Nothing}
  call_id::Int
  fingerprint::String
  scope_level::Int
  line::Int
  type::MPType
end
CallFactor(identifier, arguments, call_id, line) =
  CallFactor(identifier, arguments, Vector{String}(), nothing, call_id, "", 0, line, MUndefined)

Call = Union{CallFactor,CallStatement}

relational_operators = [
  equals,
  not_equal,
  less_than,
  less_than_or_equal,
  greater_than,
  greater_than_or_equal
]

adding_operators = [
  plus,
  minus,
  kw_or
]

mult_operators = [
  times,
  divide,
  modulo,
  kw_and
]

literals = [
  int_literal,
  string_literal,
  real_literal,
  kw_true,
  kw_false
]

mutable struct ParsingContext
  tokens::Vector{Token}
  index::Int
  signatures::Vector{Subroutine}
  string_literals::Vector{LiteralFactor}
  all_var_names_and_types::Vector{Tuple{String,MPType}}
  id_counter::Int
end
ParsingContext(tokens::Vector{Token}) =
  ParsingContext(tokens, 1, Vector{Subroutine}(), Vector{LiteralFactor}(), Vector{Tuple{String,MPType}}(), 0)

function create_node_id(pc::ParsingContext)
  id = pc.id_counter
  pc.id_counter += 1
  return id
end

function next_token(pc::ParsingContext)
  if pc.index > length(pc.tokens)
    throw(SyntaxException(
      "Failed to parse program. Do all your statements have a terminating semicolon?"))
  end
  return pc.tokens[pc.index]
end

nextclass(pc::ParsingContext) = next_token(pc::ParsingContext).class

# Convenience function. Returns the token, its class and its line number.
function token_class_line(pc::ParsingContext)
  token = next_token(pc::ParsingContext)
  return token, token.class, token.line
end

"""
Consumes the next token, checking that the token class matches.
"""
function match_term(terminal::TokenClass, pc::ParsingContext, current_unit::String)
  DEBUG && println("match_term called in $(current_unit) with terminal $(terminal), next is $(nextclass(pc))")
  token, class, line = token_class_line(pc)
  does_match = terminal == class
  if !does_match
    if terminal == semicolon && class == eoi
      throw(SyntaxException(
        "Unexpected end of input. Did you forget a semicolon?"))
    elseif terminal == identifier && class ∈ values(keywords)
      throw(SyntaxException(
        "Expected a variable identifier but got keyword \"$(token.lexeme)\" on line $(line)."))
    else
      throw(SyntaxException(
        "Failed to parse $(current_unit) on or around line $(line)."))
    end
  end
  pc.index += 1
  return does_match
end

"""
The main function of the parser. Maintains an index ("next") to point
at the token to be processed next. Builds the AST by calling mutually
recursive parsing functions, starting with statements().
"""
parse_input(input::Vector{Token}) = parse_input(input, ParsingContext(input))

function parse_input(input::Vector{Token}, pc::ParsingContext)
  if length(input) < 2
    throw(SyntaxException(
      "The empty string is not a valid program."
    ))
  end
  program(pc)
end

function program(pc::ParsingContext)
  current_unit = "program"
  DEBUG && println("this is $(current_unit), next is ", next_token(pc))
  match_term(kw_program, pc, current_unit)
  match_term(identifier, pc, current_unit)
  match_term(semicolon, pc, current_unit)
  token, class, line = token_class_line(pc)
  defs = Definitions(Vector{Subroutine}(), line)
  if class ∈ [kw_function, kw_procedure]
    defs::Definitions = definitions(pc)
    push!(pc.signatures, defs.defs...)
  end
  main = block(pc)
  match_term(dot, pc, current_unit)
  return Program(defs, main, line)
end

function procedure(pc::ParsingContext)
  current_unit = "procedure"
  token, class, line = token_class_line(pc)
  match_term(kw_procedure, pc, current_unit)
  name = next_token(pc)
  match_term(identifier, pc, current_unit)
  match_term(open_paren, pc, current_unit)
  params = parameters(pc)
  match_term(close_paren, pc, current_unit)
  match_term(semicolon, pc, current_unit)
  body::Block = block(pc)
  body.is_subroutine_block = true
  match_term(semicolon, pc, current_unit)
  return_type = TypeOfVarOrValue(MNothing, MNothing, ImmediateInt(0), line)
  return Subroutine(name.lexeme, params, return_type, body, line)
end

function func(pc::ParsingContext)
  current_unit = "function"
  token, class, line = token_class_line(pc)
  match_term(kw_function, pc, current_unit)
  name = next_token(pc)
  match_term(identifier, pc, current_unit)
  match_term(open_paren, pc, current_unit)
  params = parameters(pc)
  match_term(close_paren, pc, current_unit)
  match_term(colon, pc, current_unit)
  ret_type = var_type(pc)
  match_term(semicolon, pc, current_unit)
  body::Block = block(pc)
  body.is_subroutine_block = true
  match_term(semicolon, pc, current_unit)
  return Subroutine(name.lexeme, params, ret_type, body, line)
end

function parameters(pc::ParsingContext)
  current_unit = "parameters"
  token, class, line = token_class_line(pc)
  params = Vector{Parameter}()
  while nextclass(pc) != close_paren
    push!(params, parameter(pc))
    if nextclass(pc) == comma
      match_term(comma, pc, current_unit)
      if nextclass(pc) == close_paren
        throw(SyntaxException(
          "A parameter list on line $(line) ends in a comma."
        ))
      end
    end
  end
  return params
end

function parameter(pc::ParsingContext)
  current_unit = "parameter"
  token, class, line = token_class_line(pc)
  is_var_par = (class == kw_var)
  is_var_par && match_term(kw_var, pc, current_unit)
  name = next_token(pc)
  match_term(identifier, pc, current_unit)
  match_term(colon, pc, current_unit)
  is_array = false
  if nextclass(pc) == kw_array
    is_array = true
  end
  v_type::TypeOfVarOrValue = var_type(pc)
  println("this is parameter, v_type is $v_type")
  return Parameter(name.lexeme, v_type.scalar_type, is_var_par, is_array, v_type.size, line)
end

function definitions(pc::ParsingContext)
  current_unit = "definitions"
  token, class, line = token_class_line(pc)
  defs = Vector{Subroutine}()
  while nextclass(pc) ∈ [kw_procedure, kw_function]
    def = (nextclass(pc) == kw_procedure ? procedure(pc) : func(pc))
    push!(defs, def)
  end
  return Definitions(defs, line)
end

function block(pc::ParsingContext)
  current_unit = "block"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  match_term(kw_begin, pc, current_unit)
  stmts = Vector{Statement}()
  while nextclass(pc) != kw_end
    push!(stmts, statement(pc))
    if nextclass(pc) == semicolon
      match_term(semicolon, pc, current_unit)
    end
  end
  match_term(kw_end, pc, current_unit)
  return Block(stmts, line)
end

function statement(pc::ParsingContext)
  current_unit = "statement"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  if class == kw_var
    return var_declaration(pc)
  end
  if class == kw_begin
    return block(pc)
  end
  if class == kw_if
    return if_statement(pc)
  end
  if class == kw_while
    return while_statement(pc)
  end
  return simple_statement(pc)
end

function var_declaration(pc::ParsingContext)
  current_unit = "var_declaration"
  token, class, line = token_class_line(pc)
  match_term(kw_var, pc, current_unit)
  names = Vector{Token}()
  while nextclass(pc) != colon
    push!(names, next_token(pc))
    match_term(identifier, pc, current_unit)
    if nextclass(pc) == comma
      match_term(comma, pc, current_unit)
      if nextclass(pc) == colon
        throw(SyntaxException(
          "Did not expect comma before colon in declaration on line $(line)."
        ))
      end
    end
  end
  match_term(colon, pc, current_unit)
  v_type = var_type(pc)

  # Collect name-type pairs into parsing context
  true_type = v_type.true_type
  for name in names
    entry = (name.lexeme, true_type)
    if entry ∉ pc.all_var_names_and_types
      push!(pc.all_var_names_and_types, entry)
    end
  end

  return Declaration(v_type, names, line)
end

function simple_statement(pc::ParsingContext)
  current_unit = "simple statement"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  if class == kw_return
    return return_statement(pc)
  end
  if class == kw_read
    return read_statement(pc)
  end
  if class == kw_writeln
    return write_statement(pc)
  end
  if class == kw_assert
    return assert_statement(pc)
  end
  if class == identifier
    # The statement is a call or an assignment.
    # Disambiguate by using lookahead.
    if pc.tokens[pc.index+1].class == open_paren
      return call_statement(pc)
    end
    if pc.tokens[pc.index+1].class ∈ [assign, open_sqr_bracket]
      return assignment(pc)
    end
  end
  throw(SyntaxException(
    "Failed to parse statement on line $(line).")
  )
end

function return_statement(pc::ParsingContext)
  current_unit = "return statement"
  token, class, line = token_class_line(pc)
  match_term(kw_return, pc, current_unit)
  if nextclass(pc) ∈ [semicolon, kw_end]
    return Return(Nothing, line)
  end
  return Return(expr(pc), line)
end

function assignment(pc::ParsingContext)
  current_unit = "assignment"
  token, class, line = token_class_line(pc)
  var::Variable = variable(pc)
  match_term(assign, pc, current_unit)
  value = expr(pc)
  return Assignment(var.identifier, var.is_array_access, var.array_index, value, line)
end

# A function or procedure call as a statement.
# The return value is discarded.
function call_statement(pc::ParsingContext)
  current_unit = "call statement"
  token, class, line = token_class_line(pc)
  match_term(identifier, pc, current_unit)
  match_term(open_paren, pc, current_unit)
  args = arguments(pc, token.lexeme)
  match_term(close_paren, pc, current_unit)
  return CallStatement(token.lexeme, args, create_node_id(pc), line)
end

function write_statement(pc::ParsingContext)
  current_unit = "write statement"
  token, class, line = token_class_line(pc)
  match_term(kw_writeln, pc, current_unit)
  match_term(open_paren, pc, current_unit)
  args = arguments(pc, "writeln")
  match_term(close_paren, pc, current_unit)
  return Write(args, line)
end

function if_statement(pc::ParsingContext)
  current_unit = "if_statement"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  match_term(kw_if, pc, current_unit)
  condition = expr(pc)
  match_term(kw_then, pc, current_unit)
  then_stmt = statement(pc)
  if nextclass(pc) != kw_else
    return IfThen(condition, then_stmt, line)
  end
  match_term(kw_else, pc, current_unit)
  else_stmt = statement(pc)
  return IfThenElse(condition, then_stmt, else_stmt, line)
end

function while_statement(pc::ParsingContext)
  current_unit = "while_statement"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  match_term(kw_while, pc, current_unit)
  condition = expr(pc)
  match_term(kw_do, pc, current_unit)
  do_stmt = statement(pc)
  return While(condition, do_stmt, line)
end

function arguments(pc::ParsingContext, subroutine_name::String)
  current_unit = "arguments"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  subroutine = getfirst(sr -> sr.name == subroutine_name, pc.signatures)
  if subroutine === nothing  # No signature found for subroutine
    if subroutine_name ∈ ["read", "writeln"]
      return read_writeln_args(pc, subroutine_name)
    end
  end
  args = Vector{Value}()
  while nextclass(pc) != close_paren
    push!(args, expr(pc))
    if nextclass(pc) == comma match_term(comma, pc, current_unit) end
  end
  return args
end

function read_writeln_args(pc, subroutine_name::String)
  current_unit = "read_writeln_args"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  args = Vector{Value}()
  while nextclass(pc) != close_paren
    push!(args, expr(pc))
    if nextclass(pc) == comma match_term(comma, pc, current_unit) end
  end
  return args
end

function expr(pc::ParsingContext)
  current_unit = "expression"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  left_simple = simple_expr(pc)
  token, class, line = token_class_line(pc)
  if class ∈ relational_operators
    match_term(class, pc, current_unit)
    right_simple = simple_expr(pc)
    # return RelationalExpression(left_simple, right_simple, token, line)
    return BinaryOperation(class, left_simple, right_simple, line)
  end
  return left_simple
end

function simple_expr(pc::ParsingContext)
  current_unit = "simple expression"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  first_term_sign = Token(plus, "+", line)
  if class ∈ [plus, minus]
    first_term_sign = token
    match_term(class, pc, current_unit)
  end
  first_term = term(pc)
  if first_term_sign.class == minus
    first_term = UnaryOperation(minus, first_term, first_term_sign.line)
  end
  ret_expr = first_term
  while nextclass(pc) ∈ adding_operators
    operation = next_token(pc)
    match_term(operation.class, pc, current_unit)
    new_term = term(pc)
    ret_expr = BinaryOperation(operation.class, ret_expr, new_term, operation.line)
  end
  return ret_expr
end

function term(pc::ParsingContext)
  current_unit = "term"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  factors = Array{Tuple{Factor,Token},1}()
  first_factor = factor(pc)
  ret_term = first_factor
  # push!(factors, (first_factor, Token(identity_op, "identity", line)))
  while nextclass(pc) ∈ mult_operators
    operator = next_token(pc)
    match_term(operator.class, pc, current_unit)
    ret_term = BinaryOperation(operator.class, ret_term, factor(pc), operator.line)
    # push!(factors, (factor(pc), operator))
  end
  # return Term(factors, line)
  return ret_term
end

function factor(pc::ParsingContext)
  current_unit = "factor"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  fact = Nothing
  if class == open_paren
    fact = paren_factor(pc)
  elseif class == kw_not
    fact = not_factor(pc)
  elseif class ∈ literals
    fact = literal_factor(pc)
  elseif class == identifier
    # The factor is a call or a variable.
    # Disambiguate by using lookahead.
    if pc.tokens[pc.index+1].class == open_paren
      fact = call_factor(pc)
    else
      fact = variable_factor(pc)
    end
  end
  token, class, line = token_class_line(pc)
  if class == dot
    match_term(dot, pc, current_unit)
    match_term(kw_size, pc, current_unit)
    return SizeFactor(fact, line)
  end
  return fact
end

function call_factor(pc::ParsingContext)
  current_unit = "call_factor"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  id = token
  match_term(identifier, pc, current_unit)
  match_term(open_paren, pc, current_unit)
  args = arguments(pc, id.lexeme)
  match_term(close_paren, pc, current_unit)
  return CallFactor(id.lexeme, args, create_node_id(pc), line)
end

function variable(pc::ParsingContext)
  current_unit = "variable"
  token, class, line = token_class_line(pc)
  id = token
  match_term(identifier, pc, current_unit)
  token, class, _ = token_class_line(pc)
  idx = ImmediateInt(-1)
  is_array_access = false
  if class == open_sqr_bracket
    is_array_access = true
    match_term(open_sqr_bracket, pc, current_unit)
    idx = expr(pc)
    match_term(close_sqr_bracket, pc, current_unit)
  end
  return Variable(id.lexeme, is_array_access, idx)
end

function variable_factor(pc::ParsingContext)
  current_unit = "variable_factor"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  var::Variable = variable(pc)
  if var.is_array_access
    return ArrayAccessFactor(var.identifier, var.array_index, line)
  end
  return VariableFactor(var.identifier, line)
end

# A boolean negation ("not") factor 
function not_factor(pc::ParsingContext)
  current_unit = "not_factor"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  match_term(kw_not, pc, current_unit)
  return NotFactor(factor(pc), line)
end

function literal_factor(pc::ParsingContext)
  current_unit = "literal_factor"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  match_term(class, pc, current_unit)
  unique_id = "@_$(pc.index)_"
  factor = LiteralFactor(token, unique_id, line)
  if class == string_literal push!(pc.string_literals, factor) end
  return factor
end

# Parenthesized expression as factor
function paren_factor(pc::ParsingContext)
  current_unit = "paren_factor"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  match_term(open_paren, pc, current_unit)
  expression = expr(pc)
  match_term(close_paren, pc, current_unit)
  return ParenFactor(expression, line)
end

function var_type(pc::ParsingContext)
  current_unit = "var_type"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  if class == kw_array
    match_term(kw_array, pc, current_unit)
    match_term(open_sqr_bracket, pc, current_unit)
    len = expr(pc)
    match_term(close_sqr_bracket, pc, current_unit)
    match_term(kw_of, pc, current_unit)
    s_type = next_token(pc)
    match_term(s_type.class, pc, current_unit)
    scalar_type = token_class_to_scalar_type[s_type.class]
    return TypeOfVarOrValue(scalar_type, scalar_to_array_type[scalar_type], len, line)
  end
  match_term(class, pc, current_unit)
  return TypeOfVarOrValue(token_class_to_scalar_type[class], token_class_to_scalar_type[class], ImmediateInt(0), line)
end

# function var_as_arg(pc::ParsingContext)
#   current_unit = "var_as_par"
#   token, class, line = token_class_line(pc)
#   DEBUG && println("this is $(current_unit), next is ", token)
#   match_term(identifier, pc, current_unit)
#   return VarAsArgument(token.lexeme, line)
# end

# parse_input(source::String) = parse_input(scan_input(source))
