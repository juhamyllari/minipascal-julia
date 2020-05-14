include("Scanner.jl")

struct SyntaxException <: Exception
  msg::String
end

@enum MPType begin
  MInt
  MReal
  MBool
  MString
  MIntRef
  MRealRef
  MBoolRef
  MStringRef
  MNothing
  MError
  MPassed
  MUndefined
end

token_class_to_nonref_type = Dict(
  kw_int => MInt,
  kw_real => MReal,
  kw_bool => MBool,
  kw_string => MString
)

token_class_to_ref_type = Dict(
  kw_int => MIntRef,
  kw_real => MRealRef,
  kw_bool => MBoolRef,
  kw_string => MStringRef
)

abstract type Node end
abstract type Statement <: Node end
abstract type Value <: Node end
abstract type Factor <: Value end

mutable struct ImmediateInt <: Value
  value::Int
  type::MPType
  ImmediateInt(value) = new(value, MUndefined)
end

mutable struct ImmediateReal <: Value
  value::Float64
  type::MPType
  ImmediateReal(value) = new(value, MUndefined)
end

mutable struct ImmediateString <: Value
  value::String
  type::MPType
  ImmediateString(value) = new(value, MUndefined)
end

mutable struct ImmediateBool <: Value
  value::Bool
  type::MPType
  ImmediateBool(value) = new(value, MUndefined)
end

mutable struct Block <: Node
  statements::Vector{Statement}
  line::Int
  type::MPType
  Block(statements, line) = new(statements, line, MUndefined)
end

# mutable struct SimpleType <: VarType
#   vartype::MPType
#   line::Int
#   type::MPType
#   SimpleType(vartype, line) = new(vartype, line, MUndefined)
# end

# mutable struct ArrayType <: VarType
#   vartype::MPType
#   length::Value
#   line::Int
#   ArrayType(vartype, length, line) = new(vartype, length, line, MUndefined)
# end

mutable struct VarType <: Node
  var_type::MPType
  size::Value
  line::Int
  type::MPType
  VarType(var_type, size, line) = new(var_type, size, line, MUndefined) 
end

mutable struct Parameter <: Node
  name::String
  var_type::MPType
  size::Value
  is_ref::Bool
  line::Int
  type::MPType
  Parameter(name, var_type, size, is_ref, line) = new(name, var_type, size, is_ref, line, MUndefined)
end

mutable struct Parameters <: Node
  params::Vector{Parameter}
  line::Int
  type::MPType
  Parameters(params, line) = new(params, line, MUndefined)
end

# Function or procedure.
mutable struct Subroutine <: Node
  name::Token
  params::Parameters
  ret_type::VarType
  body::Block
  line::Int
  type::MPType
  Subroutine(name, params, ret_type, body, line) = new(name, params, ret_type, body, line, MUndefined) 
end

mutable struct Definitions <: Node
  defs::Vector{Subroutine}
  line::Int
  type::MPType
  Definitions(defs, line) = new(defs, line, MUndefined)
end

mutable struct Program <: Node
  definitions::Definitions
  main::Block
  line::Int
  type::MPType
  Program(definitions, main, line) = new(definitions, main, line, MUndefined)
end

mutable struct Arguments <: Node
  arguments::Vector{Value}
  line::Int
  type::MPType
  Arguments(arguments, line) = new(arguments, line, MUndefined)
end

mutable struct CallStatement <: Statement
  ident::Token
  arguments::Arguments
  line::Int
  type::MPType
  CallStatement(ident, arguments, line) = new(ident, arguments, line, MUndefined)
end

mutable struct Declaration <: Statement
  names::Vector{Token}
  var_type::VarType
  line::Int
  type::MPType
  Declaration(names, var_type, line) = new(names, var_type, line, MUndefined)
end

mutable struct Assignment <: Statement
  variable::Token
  value::Value
  line::Int
  type::MPType
  Assignment(variable, value, line) = new(variable, value, line, MUndefined)
end

mutable struct If <: Statement
  predicate::Value
  then_stmt::Statement
  else_stmt::Statement
  type::MPType
end

mutable struct Read <: Statement
  variable::Token
  line::Int
  type::MPType
end

mutable struct Write <: Statement
  arguments::Arguments
  line::Int
  type::MPType
  Write(arguments, line) = new(arguments, line, MUndefined)
end

mutable struct Assert <: Statement
  argument::Value
  line::Int
  type::MPType
  Assert(argument, line) = new(argument, line, MUndefined)
end

mutable struct Return <: Statement
  value::Value
  line::Int
  type::MPType
  Return(value, line) = new(value, line, MUndefined)
end

mutable struct Term <: Value
  factors::Array{Tuple{Factor,Token},1}
  line::Int
  type::MPType
  Term(factors, line) = new(factors, line, MUndefined)
end

mutable struct SimpleExpression <: Value
  terms::Array{Tuple{Term,Token},1}
  line::Int
  type::MPType
  SimpleExpression(terms, line) = new(terms, line, MUndefined)
end

mutable struct RelationalExpression <: Value
  left::SimpleExpression
  right::SimpleExpression
  operation::Token
  line::Int
  type::MPType
  RelationalExpression(left, right, operation, line) =
    new(left, right, operation, line, MUndefined)
end

mutable struct SizeFactor <: Factor
  array::Factor
  line::Int
  type::MPType
  SizeFactor(array, line) = new(array, line, MUndefined)
end

mutable struct NotFactor <: Factor
  argument::Factor
  line::Int
  type::MPType
  NotFactor(argument, line) = new(argument, line, MUndefined)
end

mutable struct ParenFactor <: Factor
  expression::Value
  line::Int
  type::MPType
  ParenFactor(expression, line) = new(expression, line, MUndefined)
end

mutable struct LiteralFactor <: Factor
  token::Token
  line::Int
  type::MPType
  LiteralFactor(token, line) = new(token, line, MUndefined)
end

mutable struct VariableFactor <: Factor
  identifier::Token
  line::Int
  type::MPType
  VariableFactor(identifier, line) = new(identifier, line, MUndefined)
end

mutable struct ArrayAccessFactor <: Factor
  identifier::Token
  index::Expr
  line::Int
  type::MPType
  ArrayAccessFactor(identifier, index, line) = new(identifier, index, line, MUndefined)
end

mutable struct CallFactor <: Factor
  identifier::Token
  arguments::Arguments
  line::Int
  type::MPType
  CallFactor(identifier, arguments, line) = new(identifier, arguments, line, MUndefined)
end

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

mutable struct InputCounter
  tokens::Vector{Token}
  index::Int
  InputCounter(tokens) = new(tokens, 1)
end

function next_token(ic::InputCounter)
  if ic.index > length(ic.tokens)
    throw(SyntaxException(
      "Failed to parse program. Do all your statements have a terminating semicolon?"))
  end
  return ic.tokens[ic.index]
end

nextclass(ic::InputCounter) = next_token(ic::InputCounter).class

# Convenience function. Returns the token, its class and its line number.
function token_class_line(ic::InputCounter)
  token = next_token(ic::InputCounter)
  return token, token.class, token.line
end

"""
Consumes the next token, checking that the token class matches.
"""
function match_term(terminal::TokenClass, ic::InputCounter, current_unit::String)
  DEBUG && println("match_term called in $(current_unit) with terminal $(terminal), next is $(nextclass(ic))")
  token, class, line = token_class_line(ic)
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
  ic.index += 1
  return does_match
end

"""
The main function of the parser. Maintains an index ("next") to point
at the token to be processed next. Builds the AST by calling mutually
recursive parsing functions, starting with statements().
"""
function parse_input(input::Vector{Token})

  if length(input) < 2
    throw(SyntaxException(
      "The empty string is not a valid program."
    ))
  end
  
  ic = InputCounter(input)

  program(ic)
  
end

function program(ic::InputCounter)
  current_unit = "program"
  DEBUG && println("this is $(current_unit), next is ", next_token(ic))
  match_term(kw_program, ic, current_unit)
  match_term(identifier, ic, current_unit)
  match_term(semicolon, ic, current_unit)
  token, class, line = token_class_line(ic)
  defs = Definitions(Vector{Subroutine}(), line)
  if class ∈ [kw_function, kw_procedure]
    defs = definitions(ic)
  end
  main = block(ic)
  match_term(dot, ic, current_unit)
  return Program(defs, main, line)
end

function procedure(ic::InputCounter)
  current_unit = "procedure"
  token, class, line = token_class_line(ic)
  match_term(kw_procedure, ic, current_unit)
  name = next_token(ic)
  match_term(identifier, ic, current_unit)
  match_term(open_paren, ic, current_unit)
  params = parameters(ic)
  match_term(close_paren, ic, current_unit)
  match_term(semicolon, ic, current_unit)
  body = block(ic)
  match_term(semicolon, ic, current_unit)
  return_type = SimpleType(Token(kw_nothing, "", line), line)
  return Subroutine(name, params, return_type, body, line)
end

function func(ic::InputCounter)
  current_unit = "function"
  token, class, line = token_class_line(ic)
  match_term(kw_function, ic, current_unit)
  name = next_token(ic)
  match_term(identifier, ic, current_unit)
  match_term(open_paren, ic, current_unit)
  params = parameters(ic)
  match_term(close_paren, ic, current_unit)
  match_term(colon, ic, current_unit)
  ret_type = var_type(ic)
  match_term(semicolon, ic, current_unit)
  body = block(ic)
  match_term(semicolon, ic, current_unit)
  return Subroutine(name, params, ret_type, body, line)
end

function parameters(ic::InputCounter)
  current_unit = "parameters"
  token, class, line = token_class_line(ic)
  params = Vector{Parameter}()
  while nextclass(ic) != close_paren
    push!(params, parameter(ic))
    if nextclass(ic) == comma
      match_term(comma, ic, current_unit)
      if nextclass(ic) == close_paren
        throw(SyntaxException(
          "A parameter list on line $(line) ends in a comma."
        ))
      end
    end
  end
  return Parameters(params, line)
end

function parameter(ic::InputCounter)
  current_unit = "parameter"
  token, class, line = token_class_line(ic)
  is_ref = (class == kw_var)
  is_ref && match_term(kw_var, ic, current_unit)
  name = next_token(ic)
  match_term(identifier, ic, current_unit)
  match_term(colon, ic, current_unit)
  v_type::VarType = var_type(ic)
  return Parameter(name.lexeme, v_type.var_type, v_type.size, is_ref, line)
end

function definitions(ic::InputCounter)
  current_unit = "definitions"
  token, class, line = token_class_line(ic)
  defs = Vector{Subroutine}()
  while nextclass(ic) ∈ [kw_procedure, kw_function]
    def = (nextclass(ic) == kw_procedure ? procedure(ic) : func(ic))
    push!(defs, def)
  end
  return Definitions(defs, line)
end

function block(ic::InputCounter)
  current_unit = "block"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  match_term(kw_begin, ic, current_unit)
  stmts = Vector{Node}()
  while nextclass(ic) != kw_end
    push!(stmts, statement(ic))
    if nextclass(ic) == semicolon
      match_term(semicolon, ic, current_unit)
    end
  end
  match_term(kw_end, ic, current_unit)
  return Block(stmts, line)
end

function statement(ic::InputCounter)
  current_unit = "statement"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  if class == kw_var
    return var_declaration(ic)
  end
  if class == kw_begin
    return block(ic)
  end
  if class == kw_if
    return if_statement(ic)
  end
  if class == kw_while
    return while_statement(ic)
  end
  return simple_statement(ic)
end

function var_declaration(ic::InputCounter)
  current_unit = "var_declaration"
  token, class, line = token_class_line(ic)
  match_term(kw_var, ic, current_unit)
  names = Vector{Token}()
  while nextclass(ic) != colon
    push!(names, next_token(ic))
    match_term(identifier, ic, current_unit)
    if nextclass(ic) == comma
      match_term(comma, ic, current_unit)
      if nextclass(ic) == colon
        throw(SyntaxException(
          "Did not expect comma before colon in declaration on line $(line)."
        ))
      end
    end
  end
  match_term(colon, ic, current_unit)
  v_type = var_type(ic)
  return Declaration(names, v_type, line)
end

function simple_statement(ic::InputCounter)
  current_unit = "simple statement"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  if class == kw_return
    return return_statement(ic)
  end
  if class == kw_read
    return read_statement(ic)
  end
  if class == kw_writeln
    return write_statement(ic)
  end
  if class == kw_assert
    return assert_statement(ic)
  end
  if class == identifier
    # The statement is a call or an assignment.
    # Disambiguate by using lookahead.
    if ic.tokens[ic.index+1].class == open_paren
      return call_statement(ic)
    end
    if ic.tokens[ic.index+1].class ∈ [assign, open_sqr_bracket]
      return assignment(ic)
    end
  end
  throw(SyntaxException(
    "Failed to parse statement on line $(line).")
  )
end

function return_statement(ic::InputCounter)
  current_unit = "return statement"
  token, class, line = token_class_line(ic)
  match_term(kw_return, ic, current_unit)
  if nextclass(ic) ∈ [semicolon, kw_end]
    return Return(Nothing, line)
  end
  return Return(expr(ic), line)
end

function assignment(ic::InputCounter)
  current_unit = "assignment"
  token, class, line = token_class_line(ic)
  match_term(identifier, ic, current_unit)
  match_term(assign, ic, current_unit)
  value = expr(ic)
  return Assignment(token, value, line)
end

# A function or procedure call as a statement.
# The return value is discarded.
function call_statement(ic::InputCounter)
  current_unit = "call statement"
  token, class, line = token_class_line(ic)
  match_term(identifier, ic, current_unit)
  match_term(open_paren, ic, current_unit)
  args = arguments(ic)
  match_term(close_paren, ic, current_unit)
  return CallStatement(token, args, line)
end

function write_statement(ic::InputCounter)
  current_unit = "write statement"
  token, class, line = token_class_line(ic)
  match_term(kw_writeln, ic, current_unit)
  match_term(open_paren, ic, current_unit)
  args = arguments(ic)
  match_term(close_paren, ic, current_unit)
  return Write(args, line)
end

function arguments(ic::InputCounter)
  current_unit = "arguments"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  args = Vector{Value}()
  while nextclass(ic) ∉ [close_paren, semicolon]
    push!(args, expr(ic))
    nextclass(ic) == comma && match_term(comma, ic, current_unit)
  end
  return Arguments(args, line)
end

function expr(ic::InputCounter)
  current_unit = "expression"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  left_simple = simple_expr(ic)
  token, class, line = token_class_line(ic)
  if class ∈ relational_operators
    match_term(class, ic, current_unit)
    right_simple = simple_expr(ic)
    return RelationalExpression(left_simple, right_simple, token, line)
  end
  return left_simple
end

function simple_expr(ic::InputCounter)
  current_unit = "simple expression"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  sign = Token(plus, "+", line)
  if class ∈ [plus, minus]
    sign = class
    match_term(class, ic, current_unit)
  end
  terms = Array{Tuple{Term,Token},1}()
  first_term = term(ic)
  push!(terms, (first_term, sign))
  while nextclass(ic) ∈ adding_operators
    operation = next_token(ic)
    match_term(operation.class, ic, current_unit)
    new_term = term(ic)
    push!(terms, (new_term, operation))
  end
  return SimpleExpression(terms, line)
end

function term(ic::InputCounter)
  current_unit = "term"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  factors = Array{Tuple{Factor,Token},1}()
  first_factor = factor(ic)
  push!(factors, (first_factor, Token(identity_op, "identity", line)))
  while nextclass(ic) ∈ mult_operators
    operator = next_token(ic)
    match_term(operator.class, ic, current_unit)
    push!(factors, (factor(ic), operator))
  end
  return Term(factors, line)
end

function factor(ic::InputCounter)
  current_unit = "factor"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  fact = Nothing
  if class == open_paren
    fact = paren_factor(ic)
  elseif class == kw_not
    fact = not_factor(ic)
  elseif class ∈ literals
    fact = literal_factor(ic)
  elseif class == identifier
    # The factor is a call or a variable.
    # Disambiguate by using lookahead.
    if ic.tokens[ic.index+1].class == open_paren
      fact = call_factor(ic)
    else
      fact = variable_factor(ic)
    end
  end
  token, class, line = token_class_line(ic)
  if class == dot
    match_term(dot, ic, current_unit)
    match_term(kw_size, ic, current_unit)
    return SizeFactor(fact, line)
  end
  return fact
end

function call_factor(ic::InputCounter)
  current_unit = "call_factor"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  id = token
  match_term(identifier, ic, current_unit)
  match_term(open_paren, ic, current_unit)
  args = arguments(ic)
  match_term(close_paren, ic, current_unit)
  return CallFactor(id, args, line)
end

function variable_factor(ic::InputCounter)
  current_unit = "variable_factor"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  id = token
  match_term(identifier, ic, current_unit)
  token, class, _ = token_class_line(ic)
  if class == open_sqr_bracket
    match_term(open_sqr_bracket, ic, current_unit)
    idx = expr(ic)
    match_term(close_sqr_bracket, ic, current_unit)
    return ArrayAccessFactor(id, idx, line)
  end
  return VariableFactor(id, line)
end

# A boolean negation ("not") factor 
function not_factor(ic::InputCounter)
  current_unit = "not_factor"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  match_term(kw_not, ic, current_unit)
  return NotFactor(factor(ic), line)
end

function literal_factor(ic::InputCounter)
  current_unit = "literal_factor"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  match_term(class, ic, current_unit)
  return LiteralFactor(token, line)
end

# Parenthesized expression as factor
function paren_factor(ic::InputCounter)
  current_unit = "paren_factor"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  match_term(open_paren, ic, current_unit)
  expression = expr(ic)
  match_term(close_paren, ic, current_unit)
  return ParenFactor(expression, line)
end

function var_type(ic::InputCounter)
  current_unit = "var_type"
  token, class, line = token_class_line(ic)
  DEBUG && println("this is $(current_unit), next is ", token)
  if class == kw_array
    match_term(kw_array, ic, current_unit)
    match_term(open_sqr_bracket, ic, current_unit)
    len = expr(ic)
    match_term(close_sqr_bracket, ic, current_unit)
    match_term(kw_of, ic, current_unit)
    s_type = next_token(ic)
    match_term(identifier, ic, current_unit)
    return VarType(token_class_to_ref_type[s_type.class], len, line)
  end
  match_term(class, ic, current_unit)
  return VarType(token_class_to_nonref_type[class], ImmediateInt(0), line)
end

parse_input(source::String) = parse_input(scan_input(source))
