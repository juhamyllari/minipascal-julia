struct LexicalException <: Exception
  msg::String
end

@enum TokenClass begin
  ident
  times
  plus
  minus
  divide
  modulo
  identity_op
  equals
  not_equal
  less_than
  greater_than
  less_than_or_equal
  greater_than_or_equal
  open_paren
  close_paren
  open_sqr_bracket
  close_sqr_bracket
  rng
  semicolon
  colon
  comma
  dot
  doublequote
  int_literal
  string_literal
  real_literal
  assign
  kw_program
  kw_begin
  kw_end
  kw_procedure
  kw_function
  kw_return
  kw_var
  kw_while
  kw_for
  kw_if
  kw_then
  kw_else
  kw_in
  kw_do
  kw_read
  kw_writeln
  kw_array
  kw_of
  kw_size
  kw_and
  kw_or
  kw_not
  kw_int
  kw_real
  kw_string
  kw_bool
  kw_assert
  kw_true
  kw_false
  eoi
  undefined
end

whitespace = [' ', '\t', '\n']
end_of_input_symbol = '$'
delimiters = union(whitespace, end_of_input_symbol)
ident_or_kw_initial = union('a':'z', 'A':'Z')
ident_or_kw_body = union(ident_or_kw_initial, '0':'9', '_')
keywords = Dict([
  "program" => kw_program,
  "begin" => kw_begin,
  "end" => kw_end,
  "procedure" => kw_procedure,
  "function" => kw_function,
  "return" => kw_return,
  "var" => kw_var,
  "while" => kw_while,
  "for" => kw_for,
  "if" => kw_if,
  "then" => kw_then,
  "else" => kw_else,
  "in" => kw_in,
  "do" => kw_do,
  "read" => kw_read,
  "writeln" => kw_writeln,
  "array" => kw_array,
  "of" => kw_of,
  "size" => kw_size,
  "int" => kw_int,
  "int" => kw_real,
  "string" => kw_string,
  "bool" => kw_bool,
  "assert" => kw_assert,
  "true" => kw_true,
  "false" => kw_false,
  "and" => kw_and,
  "or" => kw_or,
  "not" => kw_not
  ])
symbol_initials = ['*', '+', '-', '/', '(', ')', '[', ']', '.', ';', ':', '!', '=', '<', '>', '&', '%'] 
digits = '0':'9'
unary_ops = Dict(
#   log_not => '!'
)
binary_ops = Dict(
  times => '*',
  plus => '+',
  minus => '-',
  divide => '/',
  equals => '=',
  less_than => '<',
  modulo => '%'
)
syntactic_symbols = Dict(
  open_paren => '(',
  close_paren => ')',
  open_sqr_bracket => '[',
  close_sqr_bracket => ']',
  colon => ':',
  semicolon => ';',
  comma => ',',
  dot => '.',
  doublequote => '"'
)
symbol_to_character = union(unary_ops, binary_ops, syntactic_symbols)
character_to_symbol = Dict([(sym => op) for (op, sym) ∈ symbol_to_character])

mutable struct Token
  class::TokenClass
  lexeme::String
  line::Int
end

Token(class::TokenClass, lexeme::String) = Token(class, lexeme, 0)

"""
The main function of the scanner. The "next" index points at the
character to scan next. The outer while loop skips whitespace, keeps
track of the line number and the comment nesting level, calls the
getToken function, stores the resulting tokens and increments the index.
"""
function scanInput(input::AbstractString, next = 1)
  input *= end_of_input_symbol
  tokens = Array{Token,1}()
  lineNumber = 1
  while next <= length(input)
    commentNesting = 0
    while (input[next] in whitespace
      || commentNesting > 0
      || (input[next] == '/' && input[next+1] == '/')
      || (input[next] == '/' && input[next+1] == '*'))
      if input[next] == '\n'
        lineNumber += 1
        next += 1
      elseif input[next] == '/' && input[next+1] == '/' && commentNesting == 0
        while input[next] != '\n' && (next < length(input) - 1) next += 1 end
      elseif input[next] == '/' && input[next+1] == '*'
        commentNesting += 1
        next += 2
      elseif input[next] == '*' && input[next+1] == '/'
        commentNesting -= 1
        next += 2
      else
        next += 1
      end
    end
    
    if next <= length(input)
      token, next = getToken(input, next, lineNumber)
      token.line = lineNumber
      push!(tokens, token)
    end
  end
  return tokens
end

"""
Looks at the first character of a potential lexeme and
calls the corresponding function to produce a token.
"""
function getToken(input, next, lineNumber)
  c = input[next]
  if c ∈ ident_or_kw_initial
    return getIdentOrKw(input, next, lineNumber)
  end
  if c ∈ symbol_initials
    return getOperator(input, next, lineNumber)
  end
  if c ∈ digits
    return getInteger(input, next, lineNumber)
  end
  if c == '"'
    return getString(input, next, lineNumber)
  end
  if c == '$'
    return Token(eoi, string(end_of_input_symbol)), next+1
  end
  throw(LexicalException(
    "Characted $(c) on or near line $(lineNumber) is not part of a legal token."))
end

function getIdentOrKw(input, next, lineNumber)
  initial = next
  while input[next] ∉ union(delimiters, symbol_initials) next += 1 end
  str = input[initial:next-1]
  token = str ∈ keys(keywords) ? Token(keywords[str], str) : Token(ident, str)
  return token, next
end

function getOperator(input, next, lineNumber)
  # if input[next] == '.'
  #   input[next+1] != '.' && throw(LexicalException(
  #     "A single dot (on line $(lineNumber)) is not a valid token. Did you mean '..'?"))
  #   return Token(rng, ".."), next+2
  # end
  if input[next] == ':'
    input[next+1] == '=' && return Token(assign, ":="), next+2
    return Token(colon, ":"), next+1
  end
  if input[next] == '<'
    input[next+1] == '=' && return Token(less_than_or_equal, "<="), next+2
    input[next+1] == '>' && return Token(not_equal, "<>"), next+2
    return Token(less_than, "<"), next+1
  end
  if input[next] == '>'
    input[next+1] == '=' && return Token(greater_than_or_equal, ">="), next+2
    return Token(greater_than, ">"), next+1
  end
  tokenClass = character_to_symbol[input[next]]
  return Token(tokenClass, string(input[next])), next+1
end

function getInteger(input, next, lineNumber)
  initial = next
  while input[next] ∈ digits next += 1 end
  return Token(int_literal, input[initial:next-1]), next
end

function getString(input, next, lineNumber)
  initial = next
  next += 1
  str = Array{Char,1}()
  while (input[next] != '"' ||
    (input[next] == '"' && input[next-1] == '\\'))
    if input[next] == '\\'
      next += 1
      if input[next] == 'n'
        push!(str, '\n')
      end
      if input[next] == 't'
        push!(str, '\t')
      end
      if input[next] == '"'
        push!(str, '"')
      end
      if input[next] == '\\'
        push!(str, '\\')
      end
      next += 1
    else
      push!(str, input[next])
      next += 1
    end
    next >= length(input) && throw(LexicalException("Reached the end of the program while
      scanning a string literal. Did you forget the closing quote?"))
  end
  return Token(string_literal, join(str)), next+1
end
