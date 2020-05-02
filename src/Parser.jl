include("Scanner.jl")

struct SyntaxException <: Exception
  msg::String
end

@enum MPType begin
  MInt
  MReal
  MBool
  MString
  MError
  MPassed
  MUndefined
end

abstract type Node end
abstract type Statement <: Node end
abstract type Value <: Node end
abstract type VarType <: Node end
abstract type Definition <: Node end
abstract type Factor <: Value end

mutable struct Block <: Node
  statements::Array{Statement,1}
  line::Int
  type::MPType
end

mutable struct SimpleType <: VarType
  vartype::Token
  line::Int
  type::MPType
end

mutable struct ArrayType <: VarType
  vartype::Token
  length::Value
  line::Int
  type::MPType
end

mutable struct Parameter <: Node
  name::Token
  isvar::Bool
  vartype::VarType
  line::Int
  type::MPType
end

mutable struct Parameters <: Node
  params::Array{Parameter,1}
  line::Int
  type::MPType
end

mutable struct Procedure <: Definition
  name::Token
  params::Parameters
  body::Block
  line::Int
  type::MPType
end

mutable struct Func <: Definition
  name::Token
  params::Parameters
  ret_type::VarType
  body::Block
  line::Int
  type::MPType
end

mutable struct Definitions <: Node
  defs::Array{Definition,1}
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
  arguments::Array{Value,1}
  line::Int
  type::MPType
  Arguments(arguments, line) = new(arguments, line, MUndefined)
end

mutable struct CallStatement <: Statement
  ident::Token
  arguments::Arguments
  line::Int
  type::MPType
  Definitions(ident, arguments, line) = new(ident, arguments, line, MUndefined)
end

mutable struct Declaration <: Statement
  names::Array{Token,1}
  vartype::VarType
  line::Int
  type::MPType
  Declaration(names, vartype, line) = new(names, vartype, line, MUndefined)
end

mutable struct Assignment <: Statement
  variable::Token
  value::Value
  line::Int
  type::MPType
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
  factors::Array{Tuple{Factor,TokenClass},1}
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
  factor::Factor
  line::Int
  type::MPType
end

mutable struct NotFactor <: Factor
  factor::Factor
  line::Int
  type::MPType
end

mutable struct ParenFactor <: Factor
  factor::Factor
  line::Int
  type::MPType
end

mutable struct Literal <: Factor
  token::Token
  line::Int
  type::MPType
  Literal(token, line) = new(token, line, MUndefined)
end

mutable struct Operator <: Node
  token::Token
  line::Int
  type::MPType
end

mutable struct BinaryOperation <: Value
  leftOperand::Value
  operator::Token
  rightOperand::Value
  line::Int
  type::MPType
end

mutable struct UnaryOperation <: Value
  operator::Token
  operand::Value
  line::Int
  type::MPType
end

mutable struct Var <: Value
  variable::Token
  line::Int
  type::MPType
end

mutable struct LeftVal <: Node
  token::Token
  line::Int
  type::MPType
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
  real_literal
]

"""
The main function of the parser. Maintains an index ("next") to point
at the token to be processed next. Builds the AST by calling mutually
recursive parsing functions, starting with statements().
"""
function parseInput(input::Array{Token,1})
  
  # Gets the next token.
  function nxtok()
    if next > length(input)
      throw(SyntaxException(
        "Failed to parse program. Do all your statements have a terminating semicolon?"))
    end
    return input[next]
  end

  nxtclass() = nxtok().class

  # Convenience function. Returns the token, its class and its line number.
  function tok_class_line()
    tok = nxtok()
    return tok, tok.class, tok.line
  end

  """
  Consumes the next token, checking that the token class matches.
  """
  function match_term(terminal::TokenClass)
    DEBUG && println("match_term called in $(currentUnit) with terminal $(terminal), next is $(nxtclass())")
    token, class, line = tok_class_line()
    does_match = terminal == class
    if !does_match
      if terminal == semicolon && class == eoi
        throw(SyntaxException(
          "Unexpected end of input. Did you forget a semicolon?"))
      elseif terminal == ident && class ∈ values(keywords)
        throw(SyntaxException(
          "Expected a variable identifier but got keyword \"$(token.lexeme)\" on line $(line)."))
      else
        throw(SyntaxException(
          "Failed to parse $(currentUnit) on or around line $(line)."))
      end
    end
    next += 1
    return does_match
  end
  
  function program()
    currentUnit = "program"
    DEBUG && println("this is program, nxtclass is ", nxtclass())
    match_term(kw_program)
    match_term(ident)
    match_term(semicolon)
    token, class, line = tok_class_line()
    defs = Definitions(Array{Definition,1}(), line)
    if class ∈ [kw_function, kw_procedure]
      defs = definitions()
    end
    main = block()
    match_term(dot)
    return Program(defs, main, line)
  end

  function procedure()
    currentUnit = "procedure"
    token, class, line = tok_class_line()
    match_term(kw_procedure)
    name = nxtok()
    match_term(ident)
    match_term(open_paren)
    params = parameters()
    match_term(close_paren)
    match_term(semicolon)
    body = block()
    match_term(semicolon)
    return Procedure(name, params, body, line)
  end

  function function_mp()
    currentUnit = "function"
    token, class, line = tok_class_line()
    match_term(kw_function)
    name = nxtok()
    match_term(ident)
    match_term(open_paren)
    params = parameters()
    match_term(close_paren)
    match_term(colon)
    ret_type = var_type()
    match_term(semicolon)
    body = block()
    match_term(semicolon)
    return Func(name, params, ret_type, body, line)
  end

  function parameters()
    currentUnit = "parameters"
    token, class, line = tok_class_line()
    params = Array{Parameter,1}()
    while nxtclass() != close_paren
      push!(params, parameter())
      if nxtclass() == comma
        match_term(comma)
        if nxtclass() == close_paren
          throw(SyntaxException(
            "A parameter list on line $(line) ends in a comma."
          ))
        end
      end
    end
    return Parameters(params, line)
  end

  function parameter()
    currentUnit = "parameter"
    token, class, line = tok_class_line()
    isvar = (class == kw_var)
    isvar && match_term(kw_var)
    name = nxtok()
    match_term(ident)
    match_term(colon)
    v_type = var_type()
    return Parameter(name, isvar, v_type, line)
  end

  function definitions()
    currentUnit = "definitions"
    token, class, line = tok_class_line()
    defs = Array{Definition,1}()
    while nxtclass() ∈ [kw_procedure, kw_function]
      def = (nxtclass() == kw_procedure ? procedure() : func())
      push!(defs, def)
    end
    return Definitions(defs, line)
  end

  function block()
    currentUnit = "block"
    token, class, line = tok_class_line()
    DEBUG && println("this is block, nxtclass is ", class)
    match_term(kw_begin)
    stmts = Array{Node,1}()
    while nxtclass() != kw_end
      push!(stmts, statement())
      if nxtclass() == semicolon
        match_term(semicolon)
      end
    end
    match_term(kw_end)
    return Block(stmts, line)
  end

  # function statements()
  #   currentUnit = "group of statements"
  #   token, class, line = tok_class_line()
  #   DEBUG && println("this is statements, nxtype is ", class)
  #   stmts = Array{Node,1}()
  #   while nxtclass() ∉ [eoi, kw_end]
  #     push!(stmts, statement())
  #     match_term(semicolon)
  #   end
  #   return Statements(stmts, line)
  # end

  function statement()
    currentUnit = "statement"
    token, class, line = tok_class_line()
    DEBUG && println("this is statement, nxtclass is ", class)
    if class == kw_var
      return var_declaration()
    end
    if class == kw_begin
      return block()
    end
    if class == kw_if
      return if_statement()
    end
    if class == kw_while
      return while_statement()
    end
    return simple_statement()
  end

  function var_declaration()
    currentUnit = "declaration"
    token, class, line = tok_class_line()
    match_term(kw_var)
    names = Array{String,1}()
    while nxtclass() != colon
      push!(names, nxtok())
      match_term(ident)
      if nxtclass() == comma
        match_term(comma)
        if nxtclass() == colon
          throw(SyntaxException(
            "Did not expect comma before colon in declaration on line $(line)."
          ))
        end
      end
    end
    v_type = var_type()
    return Declaration(names, v_type, line)
  end

  function simple_statement()
    currentUnit = "simple statement"
    token, class, line = tok_class_line()
    DEBUG && println("this is simple statement, nxtclass is ", class)
    if class == kw_return
      return return_statement()
    end
    if class == kw_read
      return read_statement()
    end
    if class == kw_writeln
      return write_statement()
    end
    if class == kw_assert
      return assert_statement()
    end
    if class == ident
      # The statement is a call or an assignment.
      # Disambiguate by using lookahead.
      if input[next+1].class == open_paren
        call_statement()
      end
      if input[next+1].class ∈ [assign, open_sqr_bracket]
        assignment()
      end
    end
    throw(SyntaxException(
      "Failed to parse statement on line $(line).")
    )
  end

  function return_statement()
    currentUnit = "return statement"
    token, class, line = tok_class_line()
    match_term(kw_return)
    if nxtclass() ∈ [semicolon, kw_end]
      return Return(Nothing, line)
    end
    return Return(expr(), line)
  end

  function assignment()
    currentUnit = "assignment"
    token, class, line = tok_class_line()
    match_term(ident)
    match_term(assign)
    value = expr()
    return Assignment(token, value, line)
  end

  # A function or procedure call as a statement.
  # The return value is discarded.
  function call_statement()
    currentUnit = "call statement"
    token, class, line = tok_class_line()
    match_term(ident)
    match_term(open_paren)
    args = arguments()
    match_term(close_paren)
    return CallStatement(token, args, line)
  end
  
  function write_statement()
    currentUnit = "write statement"
    token, class, line = tok_class_line()
    match_term(kw_writeln)
    return Write(arguments(), line)
  end

  function arguments()
    currentUnit = "arguments"
    token, class, line = tok_class_line()
    DEBUG && println("this is arguments, nxtclass is ", class)
    args = Array{Value,1}()
    while nxtclass() ∉ [close_paren, semicolon]
      push!(args, expr())
      nxtclass() == comma && match_term(comma)
    end
    return Arguments(args, line)
  end

  function expr()
    currentUnit = "expression"
    token, class, line = tok_class_line()
    left_simple = simple_expr()
    token, class, line = tok_class_line()
    if class ∈ relational_operators
      right_simple = simple_expr()
      return RelationalExpression(left_simple, right_simple, token, line)
    end
    return left_simple
  end

  function simple_expr()
    currentUnit = "simple expression"
    token, class, line = tok_class_line()
    sign = Token(plus, "+", line)
    if class ∈ [plus, minus]
      sign = class
      match_term(class)
    end
    terms = Array{Tuple{Term,Token},1}()
    first_term = term()
    push!(terms, (first_term, sign))
    while nxtclass() ∈ adding_operators
      operation = nxtok()
      match_term(operation.class)
      new_term = term()
      push!(terms, (new_term, operation))
    end
    return SimpleExpression(terms, line)
  end

  function term()
    currentUnit = "term"
    token, class, line = tok_class_line()
    factors = Array{Tuple{Factor,TokenClass},1}()
    first_factor = factor()
    push!(factors, (first_factor, identity_op))
    while nxtclass() ∈ mult_operators
      operator = nxtclass()
      match_term(operator)
      push!(factors, (factor(), operator))
    end
    return Term(factors, line)
  end

  function factor()
    currentUnit = "term"
    token, class, line = tok_class_line()
    fact = Nothing
    if class == open_paren
      fact = paren_factor()
    elseif class == kw_not
      fact = not_factor()
    elseif class ∈ literals
      fact = literal_factor()
    elseif class == ident
      # The factor is a call or a variable.
      # Disambiguate by using lookahead.
      if input[next+1].class == open_paren
        fact = call_factor()
      else
        fact = variable_factor()
      end
    end
    token, class, line = tok_class_line()
    if class == dot
      match_term(dot)
      match_term(kw_size)
      return SizeFactor(fact, line)
    end
    return fact
  end

  function not_factor()
    token, class, line = tok_class_line()
    match_term(kw_not)
    return NotFactor(factor(), line)
  end

  function literal_factor()
    token, class, line = tok_class_line()
    match_term(class)
    return Literal(token, line)
  end

  function paren_factor()
    token, class, line = tok_class_line()
    match_term(open_paren)
    fact = factor()
    match_term(close_paren)
    return ParenFactor(fact, line)
  end

  function var_type()
    currentUnit = "type"
    token, class, line = tok_class_line()
    if class == kw_array
      match_term(kw_array)
      match_term(open_sqr_bracket)
      len = expr()
      match_term(close_sqr_bracket)
      match_term(kw_of)
      s_type = nxtok()
      match_term(ident)
      return ArrayType(SimpleType(s_type), len, line)
    end
    return SimpleType(token, line)
  end

  function unary_op()
    currentUnit = "unary operation"
    token, class, line = tok_class_line()
    DEBUG && println("this is unary_op, nxtclass is ", class)
    match_term(class)
    return Operator(token, line)
  end

  function operand()
    currentUnit = "operand"
    token, class, line = tok_class_line()
    DEBUG && println("this is operand, nxtclass is ", class)
    if class == open_paren
      match_term(open_paren)
      expression = expr()
      match_term(close_paren)
      return expression
    end
    if class == ident
      match_term(class)
      return Var(token, line)
    end
    if class ∈ [int_literal, string_literal, kw_true, kw_false]
      match_term(class)
      return Literal(token, line)
    end
    throw(SyntaxException(
      "Expected an operand on line $(line), got '$(token.lexeme)'."
    ))
  end

  function operator()
    currentUnit = "operator"
    token, class, line = tok_class_line()
    DEBUG && println("this is operator, nxtclass is ", class)
    if class ∈ keys(binary_ops)
      match_term(class)
      return Operator(token, line)
    end
    throw(SyntaxException(
      "Expected a binary operator on line $(line), got '$(token.lexeme)'."
    ))
  end

  function leftval_ident()
    currentUnit = "identifier"
    token, class, line = tok_class_line()
    DEBUG && println("this is leftval_ident, nxtclass is ", class)
    match_term(ident)
    return LeftVal(token, line)
  end

  if length(input) < 2
    throw(SyntaxException(
      "The empty string is not a valid program."
    ))
  end
  
  next = 1
  currentUnit = "program"
  program()
end

parseInput(source::String) = parseInput(scanInput(source))
