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

scalar_to_ref_type = Dict(
  MInt => MIntRef,
  MReal => MRealRef,
  MBool => MBoolRef
)

abstract type Node end
abstract type Statement <: Node end
abstract type Value <: Node end
abstract type Factor <: Value end
abstract type If <: Statement end

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

mutable struct Block <: Statement
  statements::Vector{Statement}
  line::Int
  type::MPType
  Block(statements, line) = new(statements, line, MUndefined)
end

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
  is_var_par::Bool
  is_array::Bool
  size::Value       # Array size can only be determined at run time
  line::Int
  type::MPType
  Parameter(name, var_type, is_var_par, is_array, size, line) =
    new(name, var_type, is_var_par, is_array, size, line, MUndefined)
end

mutable struct VarAsPar <: Value
  name::String
  line::Int
  type::MPType
  VarAsPar(name, line) = new(name, line, MUndefined)
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

# mutable struct Arguments <: Node
#   arguments::Vector{Value}
#   line::Int
#   type::MPType
#   Arguments(arguments, line) = new(arguments, line, MUndefined)
# end

mutable struct CallStatement <: Statement
  identifier::Token
  arguments::Vector{Value}
  line::Int
  type::MPType
  CallStatement(identifier, arguments, line) = new(identifier, arguments, line, MUndefined)
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

mutable struct IfThen <: If
  condition::Value
  then_stmt::Statement
  line::Int
  type::MPType
  IfThen(condition, then_stmt, line) = new(condition, then_stmt, line, MUndefined)
end

mutable struct IfThenElse <: If
  condition::Value
  then_stmt::Statement
  else_stmt::Statement
  line::Int
  type::MPType
  IfThenElse(condition, then_stmt, else_stmt, line) = new(condition, then_stmt, else_stmt, line, MUndefined)
end

mutable struct While <: Statement
  condition::Value
  do_stmt::Statement
  line::Int
  type::MPType
  While(condition, do_stmt, line) = new(condition, do_stmt, line, MUndefined)
end

mutable struct Read <: Statement
  variable::Token
  line::Int
  type::MPType
end

mutable struct Write <: Statement
  arguments::Vector{Value}
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
  unique_id::String  # Used to declare string literals
  line::Int
  type::MPType
  LiteralFactor(token, index, line) = new(token, index, line, MUndefined)
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
  arguments::Vector{Value}
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

mutable struct ParsingContext
  tokens::Vector{Token}
  index::Int
  signatures::Vector{Subroutine}
  string_literals::Vector{LiteralFactor}
  ParsingContext(tokens) = new(tokens, 1, Vector{Subroutine}(), Vector{LiteralFactor}())
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
  body = block(pc)
  match_term(semicolon, pc, current_unit)
  return_type = VarType(MNothing, ImmediateInt(0), line)
  return Subroutine(name, params, return_type, body, line)
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
  body = block(pc)
  match_term(semicolon, pc, current_unit)
  return Subroutine(name, params, ret_type, body, line)
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
  return Parameters(params, line)
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
  v_type = var_type(pc)
  println("this is parameter, v_type is $v_type")
  return Parameter(name.lexeme, v_type.var_type, is_var_par, is_array, v_type.size, line)
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
  stmts = Vector{Node}()
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
  return Declaration(names, v_type, line)
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
  match_term(identifier, pc, current_unit)
  match_term(assign, pc, current_unit)
  value = expr(pc)
  return Assignment(token, value, line)
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
  return CallStatement(token, args, line)
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
  subroutine = getfirst(sr -> sr.name.lexeme == subroutine_name, pc.signatures)
  if subroutine === nothing  # No signature found for subroutine
    if subroutine_name ∈ ["read", "writeln"]
      return read_writeln_args(pc, subroutine_name)
    end
    throw(SyntaxException(
      "Cannot call undefined subroutine $subroutine_name (line $line)."
    ))
  end
  parameters = (subroutine::Subroutine).params.params
  args = Vector{Value}()
  for parameter::Parameter in parameters
    if parameter.is_var_par
      push!(args, var_as_par(pc))
    else
      push!(args, expr(pc))
    end
    nextclass(pc) == comma && match_term(comma, pc, current_unit)
  end
  if nextclass(pc) != close_paren
    throw(SyntaxException(
    "Expected end of arguments, got '$(next_token(pc).lexeme)' (line $line)."
    ))
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
    return RelationalExpression(left_simple, right_simple, token, line)
  end
  return left_simple
end

function simple_expr(pc::ParsingContext)
  current_unit = "simple expression"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  sign = Token(plus, "+", line)
  if class ∈ [plus, minus]
    sign = token
    match_term(class, pc, current_unit)
  end
  terms = Array{Tuple{Term,Token},1}()
  first_term = term(pc)
  push!(terms, (first_term, sign))
  while nextclass(pc) ∈ adding_operators
    operation = next_token(pc)
    match_term(operation.class, pc, current_unit)
    new_term = term(pc)
    push!(terms, (new_term, operation))
  end
  return SimpleExpression(terms, line)
end

function term(pc::ParsingContext)
  current_unit = "term"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  factors = Array{Tuple{Factor,Token},1}()
  first_factor = factor(pc)
  push!(factors, (first_factor, Token(identity_op, "identity", line)))
  while nextclass(pc) ∈ mult_operators
    operator = next_token(pc)
    match_term(operator.class, pc, current_unit)
    push!(factors, (factor(pc), operator))
  end
  return Term(factors, line)
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
  return CallFactor(id, args, line)
end

function variable_factor(pc::ParsingContext)
  current_unit = "variable_factor"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  id = token
  match_term(identifier, pc, current_unit)
  token, class, _ = token_class_line(pc)
  if class == open_sqr_bracket
    match_term(open_sqr_bracket, pc, current_unit)
    idx = expr(pc)
    match_term(close_sqr_bracket, pc, current_unit)
    return ArrayAccessFactor(id, idx, line)
  end
  return VariableFactor(id, line)
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
    match_term(identifier, pc, current_unit)
    return VarType(token_class_to_ref_type[s_type.class], len, line)
  end
  match_term(class, pc, current_unit)
  return VarType(token_class_to_nonref_type[class], ImmediateInt(0), line)
end

function var_as_par(pc::ParsingContext)
  current_unit = "var_as_par"
  token, class, line = token_class_line(pc)
  DEBUG && println("this is $(current_unit), next is ", token)
  match_term(identifier, pc, current_unit)
  return VarAsPar(token.lexeme, line)
end

# parse_input(source::String) = parse_input(scan_input(source))