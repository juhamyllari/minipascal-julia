include("StaticAnalyzer.jl")

const INT_STR_ID = "@int_str"
const REAL_STR_ID = "@real_str"
const TRUE_STR_ID = "@true"
const FALSE_STR_ID = "@false"

add_op_and_type_to_opcode = Dict{Tuple{TokenClass,MPType},String}(
  (plus, MInt) => "add",
  (plus, MReal) => "fadd",
  (minus, MInt) => "sub",
  (minus, MReal) => "fsub",
  (kw_or, MBool) => "or"
)

mult_op_and_type_to_opcode = Dict{Tuple{TokenClass,MPType},String}(
  (times, MInt) => "mul",
  (times, MReal) => "fmul",
  (divide, MInt) => "sdiv",
  (divide, MReal) => "fdiv",
  (modulo, MInt) => "srem",
  (kw_and, MBool) => "and",
)

mptype_to_llvm_type = Dict{MPType,String}(
  MInt => "i64",
  MReal => "double",
  MBool => "i1"
)

op_and_type_to_cond = Dict{Tuple{TokenClass,MPType},String}(
  (equals, MInt) => "eq",
  (equals, MBool) => "eq",
  (equals, MReal) => "oeq",
  (not_equal, MInt) => "ne",
  (not_equal, MBool) => "ne",
  (not_equal, MReal) => "one",
  (less_than, MInt) => "slt",
  (less_than, MBool) => "ult",
  (less_than, MReal) => "olt",
  (less_than_or_equal, MInt) => "sle",
  (less_than_or_equal, MBool) => "ule",
  (less_than_or_equal, MReal) => "ole",
  (greater_than, MInt) => "sgt",
  (greater_than, MBool) => "ugt",
  (greater_than, MReal) => "ogt",
  (greater_than_or_equal, MInt) => "sge",
  (greater_than_or_equal, MBool) => "uge",
  (greater_than_or_equal, MReal) => "oge",
)

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

function generate_header(io)
  ptln(io, "")
  ptln(io, "define i32 @main(i32, i8**) #0 {")
end

function generate_strings(io)
  ptln(io, "$(INT_STR_ID) = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1")
  ptln(io, "$(REAL_STR_ID) = private unnamed_addr constant [4 x i8] c\"%f\\0A\\00\", align 1")
  ptln(io, "$(TRUE_STR_ID) = private unnamed_addr constant [6 x i8] c\"true\\0A\\00\", align 1")
  ptln(io, "$(FALSE_STR_ID) = private unnamed_addr constant [7 x i8] c\"false\\0A\\00\", align 1")
  ptln(io, "")
end

function generate_footer(io)
  ptln(io, "ret i32 0", 1) # Main returns 0
  ptln(io, "}\n") # Close main
  ptln(io, "declare i32 @printf(i8*, ...) #1")
  ptln(io, "")
  generate_writebool(io)
end

function generate_writebool(io)
  fct =
"""
define void @printbool(i1 %a) {
  entry:
    br i1 %a, label %is_true, label %is_false
  is_true:
    call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* $(TRUE_STR_ID), i32 0, i32 0), i1 %a)
    br label %end
  is_false:
    call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([7 x i8], [7 x i8]* $(FALSE_STR_ID), i32 0, i32 0), i1 %a)
    br label %end
  end:
    ret void
}
"""
  print(io, fct)
end

function generate(program::String, io)
  AST = parse_input(scan_input(program))
  DEBUG && println("parsing finished")
  static_analysis(AST)
  DEBUG && println("static analysis finished")
  generate(AST, io)
  DEBUG && println("codegen finished")
  reset_id()
end

function generate(AST::Program, io)
  st = StackST()
  generate_strings(io)
  generate(AST.definitions, st, io)
  add_predefined_names(st, io)
  generate_header(io)
  generate(AST.main, st, io)
  generate_footer(io)
  ptln(io, "")
end

function add_predefined_names(st::SymbolTable, io)
  # not implemented yet
end

function generate(d::Definitions, st::SymbolTable, io)
  for subroutine in d.defs
    generate(subroutine, st, io)
  end
end

function generate(s::Subroutine, st::SymbolTable, io)
  ret_type = s.ret_type.var_type
  llvm_ret_type = mptype_to_llvm_type[ret_type]
  params = s.params.params
  args = Vector{String}()
  for param::Parameter in params
    pushvar!(st, param.name, param.var_type, "%$(param.name)")
    push!(args, "$(mptype_to_llvm_type[param.var_type]) %$(param.name)")
  end
  argstring = join(args, ", ")
  ptln(io, "define $llvm_ret_type @$(s.name.lexeme)($argstring) {")
  generate(s.body, st, io)
  ptln(io, "}")
  pushsubroutine!(st, s.name.lexeme, Vector{MPType}(), s.ret_type.var_type)
end

function generate(b::Block, st::SymbolTable, io)
  enterscope!(st)
  for statement in b.statements
    generate(statement, st, io)
  end
  popscope!(st)
end

function generate(d::Declaration, st::SymbolTable, io)
  for name in d.names
    pushvar!(st, name.lexeme, d.var_type.var_type, "")
  end
end

function generate(r::Return, st::SymbolTable, io)
  val_id = generate(r.value, st, io)
  llvm_ret_type = mptype_to_llvm_type[r.value.type]
  ptln(io, "ret $llvm_ret_type $val_id", 1)
end

function generate(a::Assignment, st::SymbolTable, io)
  value_id = generate(a.value, st, io)
  entry = getvariable(st, a.variable.lexeme)
  entry.val_identifier = value_id
end

function generate(p::Write, st::SymbolTable, io)
  for arg in p.arguments.arguments
    arg_id = generate(arg, st, io)
    if arg.type == MBool
      ptln(io, "call void @printbool(i1 $(arg_id))", 1)
    elseif arg.type ∈ [MInt, MReal]
      arg_type = mptype_to_llvm_type[arg.type]
      envelope_id = arg.type == MInt ? INT_STR_ID : REAL_STR_ID
      pt(io, "call i32 (i8*, ...) ", 1)
      ptln(io, "@printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* $(envelope_id), i32 0, i32 0), $(arg_type) $(arg_id))")
    end
  end
end

function generate(s::SimpleExpression, st::SymbolTable, io)
  ret_type = s.terms[1][1].type
  llvm_type = mptype_to_llvm_type[ret_type]
  ret_id = generate(s.terms[1][1], st, io)
  if ret_type == MInt
    signed_id = create_id("signed_int")
    sign_op = (s.terms[1][2].class == plus ? "add" : "sub")
    ptln(io, "$(signed_id) = $(sign_op) i64 0, $(ret_id)", 1)
    ret_id = signed_id
  elseif ret_type == MReal
    signed_id = create_id("signed_real")
    sign_op = (s.terms[1][2].class == plus ? "fadd" : "fsub")
    ptln(io, "$(signed_id) = $(sign_op) double 0.0, $(ret_id)", 1)
    ret_id = signed_id
  end
  if length(s.terms) > 1
    for (term, op) in s.terms[2:end]
      term_id = generate(term, st, io)
      new_ret_id = create_id("sum_diff_or")
      opcode = add_op_and_type_to_opcode[(op.class, ret_type)]
      ptln(io, "$(new_ret_id) = $(opcode) $llvm_type $(ret_id), $(term_id)", 1)
      ret_id = new_ret_id
    end
  end
  return ret_id
end

function generate(r::RelationalExpression, st::SymbolTable, io)
  arg_type = r.left.type
  arg_llvm_type = mptype_to_llvm_type[arg_type]
  left = generate(r.left, st, io)
  right = generate(r.right, st, io)
  ret_id = create_id("cond")
  if arg_type ∈ [MInt, MReal, MBool]
    cond = op_and_type_to_cond[(r.operation.class, arg_type)]
    opcode = arg_type == MReal ? "fcmp" : "icmp"
    ptln(io, "$ret_id = $opcode $cond $arg_llvm_type $left, $right", 1)
  else
    println("rel exprs for strings or bools not implemented yet")
  end
  return ret_id
end

function generate(t::Term, st::SymbolTable, io)
  ret_type = t.factors[1][1].type
  llvm_type = mptype_to_llvm_type[ret_type]
  first_factor_id = generate(t.factors[1][1], st, io)
  ret_id = first_factor_id
  if length(t.factors) > 1
    for (factor, op) in t.factors[2:end]
      fact_id = generate(factor, st, io)
      new_ret_id = create_id("prod_quot_mod")
      opcode = mult_op_and_type_to_opcode[(op.class,ret_type)]
      ptln(io, "$(new_ret_id) = $(opcode) $(llvm_type) $(ret_id), $(fact_id)", 1)
      ret_id = new_ret_id
    end
  end
  return ret_id
end

function generate(l::LiteralFactor, st::SymbolTable, io)
  ret_id = ""
  if l.type == MInt
    ret_id = create_id("int_lit")
    ptln(io, "$(ret_id) = add i64 0, $(l.token.lexeme) ", 1)
  elseif l.type == MBool
    ret_id = create_id("bool_lit")
    if l.token.class == kw_true
      ptln(io, "$(ret_id) = or i1 1, 1 ", 1)
    else
      ptln(io, "$(ret_id) = and i1 0, 0 ", 1)
    end
  elseif l.type == MReal
    ret_id = create_id("real_lit")
    ptln(io, "$(ret_id) = fadd double 0.0, $(l.token.lexeme) ", 1)
  else
    println("codegen for non-int literals not implemented yet")
  end
  return ret_id
end

function generate(p::ParenFactor, st::SymbolTable, io)
  return generate(p.expression, st, io)
end

function generate(v::VariableFactor, st::SymbolTable, io)
  entry = getvariable(st, v.identifier.lexeme)
  return entry.val_identifier
end

function generate(n::NotFactor, st::SymbolTable, io)
  arg_id = generate(n.argument, st, io)
  ret_id = create_id("not")
  ptln(io, "$ret_id = add i1 $arg_id, 1", 1)
  return ret_id
end

function generate(c::CallFactor, st::SymbolTable, io)
  println("this is generate CallFactor, sr id lex is $(c.identifier.lexeme)")
  subroutine::SubroutineEntry = getsubroutine(st, c.identifier.lexeme)
  arg_ids = [generate(arg, st, io) for arg in c.arguments.arguments]
  arg_types = [mptype_to_llvm_type[arg.type] for arg in c.arguments.arguments ]
  arg_strs = ["$type $id" for (type, id) in zip(arg_types, arg_ids)]
  arg_string = join(arg_strs,  ", ")
  ret_id = create_id("ret")
  llvm_ret_type = mptype_to_llvm_type[subroutine.return_type]
  ptln(io, "$ret_id = call $llvm_ret_type @$(subroutine.name)($arg_string)", 1)
  return ret_id
end
