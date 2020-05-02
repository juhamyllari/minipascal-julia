include("StaticAnalyzer.jl")

const INT_STR_ID = "@int_str"

let id = 0
  global create_id(str) = begin
      id_str = "%$(str)$(id)"
      id += 1
      return id_str
    end
  
  global reset_id() = (id = 0)
end

function pt(io, str, indent)
  print(io, '\t'^indent * str)
end

pt(io, str) = pt(io, str, 0)
ptln(io, str, indent) = pt(io, str * '\n', indent)
ptln(io, str) = ptln(io, str, 0)

generate(program::String) = generate(program::String, IOBuffer())

function generateHeader(io)
  ptln(io, "$(INT_STR_ID) = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n")
  ptln(io, "define i32 @main(i32, i8**) #0 {")
end

function generateFooter(io)
  ptln(io, "ret i32 0", 1) # Main returns 0
  ptln(io, "}\n") # Close main
  ptln(io, "declare i32 @printf(i8*, ...) #1")
end

function generate(program::String, io)
  AST = parseInput(scanInput(program))
  staticAnalysis(AST)
  generateProgram(AST, io)
  reset_id()
end

function generateProgram(AST, io)
  st = SymbolTable()
  generateHeader(io)
  for s in AST.statements
    generateStatement(s, st, io)
  end
  generateFooter(io)
end

function generateStatement(p::Write, st::SymbolTable, io)
  arg_id = generateExpression(p.argument, st, io)
  pt(io, "$(create_id("print")) = call i32 (i8*, ...) ", 1)
  ptln(io, "@printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* $(INT_STR_ID), i32 0, i32 0), i32 $(arg_id))", 1)
end

function generateStatement(i::If, st::SymbolTable, io)
  
end

function generateExpression(l::Literal, st::SymbolTable, io)
  if l.token.class == int_literal
    value = parse(Int, l.token.lexeme)
    ret_id = create_id("int_lit")
    ptln(io, "$(ret_id) = add i32 $(value), 0", 1)
    return ret_id
  elseif l.token.class == string_literal
    return SValue(MString, l.token.lexeme)
  elseif l.token.class == kw_true
    return SValue(MBool, true)
  elseif l.token.class == kw_false
    return SValue(MBool, false)
  end
end

function generateExpression(node::BinaryOperation, st::SymbolTable, io)
  operation = operator_to_function[node.operator.class]
  left = generateExpression(node.leftOperand, st, io)
  right = generateExpression(node.rightOperand, st, io)
  if (operation == (÷))
    ret_id = create_id("quotient")
    ptln(io, "$(ret_id) = sdiv i32 $(left), $(right)", 1)
    return ret_id
  end
  if (operation == (%))
    ret_id = create_id("modulo")
    ptln(io, "$(ret_id) = srem i32 $(left), $(right)", 1)
    return ret_id
  end
  if operation == (+)
    ret_id = create_id("sum")
    ptln(io, "$(ret_id) = add i32 $(left), $(right)", 1)
    return ret_id
  end
  if operation == (*)
    ret_id = create_id("prod")
    ptln(io, "$(ret_id) = mul i32 $(left), $(right)", 1)
    return ret_id
  end
  return operation(left, right)
end
