include("StaticAnalyzer.jl")
include("PredefinedSubroutines.jl")

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
  MIntRef => "i64*",
  MReal => "double",
  MRealRef => "double*",
  MBool => "i1",
  MString => STRING_TYPE_ID * "*",
  MStringRef => STRING_TYPE_ID * "**",
  MBoolRef => "i1*",
  MNothing => "void",
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

mutable struct GenerationContext
  counter::Int
  symtable::SymbolTable
  string_literals::Vector{LiteralFactor}
  io
end

function create_id(gc::GenerationContext, str::String)
  id_string = "%$(str)_$(gc.counter)"
  gc.counter += 1
  return id_string
end

function reset_id_counter(gc::GenerationContext)
  gc.counter = 0
end

hasvariable(gc::GenerationContext, var_name::String) = hasvariable(gc.symtable, var_name)
getvariable(gc::GenerationContext, var_name::String) = getvariable(gc.symtable, var_name)
pushvar!(gc::GenerationContext, var_name::String, var_type::MPType, val_identifier::String) =
  pushvar!(gc.symtable, var_name::String, var_type::MPType, val_identifier::String)
getsubroutine(gc::GenerationContext, name::String) = getsubroutine(gc.symtable, name)
pushsubroutine!(gc::GenerationContext, name::String, arg_types::Vector{MPType}, return_type::MPType) =
  pushsubroutine!(gc.symtable, name::String, arg_types::Vector{MPType}, return_type::MPType)
enterscope!(gc::GenerationContext) = enterscope!(gc.symtable)
popscope!(gc::GenerationContext) = popscope!(gc.symtable)

function pt(gc::GenerationContext, str, indent)
  print(gc.io, '\t'^indent * str)
end

pt(gc, str) = pt(gc, str, 0)
ptln(gc, str, indent) = pt(gc, str * '\n', indent)
ptln(gc, str) = ptln(gc, str, 0)

generate(program::String) = generate(program::String, IOBuffer())

function generate_header(gc::GenerationContext)
  ptln(gc, "")
  ptln(gc, "define i32 @main(i32, i8**) {")
end

function generate_strings(gc::GenerationContext)
  ptln(gc, "$INT_STR_ID = private constant [3 x i8] c\"%d\\00\"")
  ptln(gc, "$REAL_STR_ID = private constant [3 x i8] c\"%f\\00\"")
  ptln(gc, "$STR_STR_ID = private constant [3 x i8] c\"%s\\00\"")
  ptln(gc, "$TRUE_STR_ID = private constant [5 x i8] c\"true\\00\"")
  ptln(gc, "$FALSE_STR_ID = private constant [6 x i8] c\"false\\00\"")
  ptln(gc, "$EMPTY_STR_ID = private constant [1 x i8] c\"\\00\"")
  generate_string_literals(gc)
  ptln(gc, "")
end

function generate_string_literals(gc::GenerationContext)
  for literal::LiteralFactor in gc.string_literals
    str = literal.token.lexeme
    len = length(str) + 1 # Account for terminating null
    ptln(gc, "$(literal.unique_id) = private constant [$len x i8] c\"$str\\00\"")
  end
end

function generate_footer(gc::GenerationContext)
  ptln(gc, "ret i32 0", 1) # Main returns 0
  ptln(gc, "}\n") # Close main
  ptln(gc, "declare i32 @printf(i8*, ...)")
  ptln(gc, "declare i32 @puts(i8*)")
  ptln(gc, "declare i8* @strcpy(i8*, i8* nocapture readonly)")
  ptln(gc, "declare i8* @strcat(i8*, i8* nocapture readonly)")
  ptln(gc, "declare i32 @strcmp(i8* nocapture, i8* nocapture)")
  ptln(gc, "")
  generate_helpers(gc)
end

function generate_types(gc)
  ptln(gc, "$STRING_TYPE_ID = type { i8*, i32 }")
end

function generate_helpers(gc)
  for helper in HELPER_FUNCTIONS
    ptln(gc, helper)
  end
end

function generate(program::String, io)
  token_sequence = scan_input(program)
  DEBUG && println("scanning finished")
  pc = ParsingContext(token_sequence)
  AST::Program = parse_input(token_sequence, pc)
  DEBUG && println("parsing finished")
  static_analysis(AST)
  DEBUG && println("static analysis finished")
  gc = GenerationContext(0, SymbolTable(), pc.string_literals, io)
  generate(AST, gc)
  DEBUG && println("codegen finished")
  reset_id_counter(gc)
end

function generate(AST::Program, gc::GenerationContext)
  st = SymbolTable()
  generate_strings(gc)
  generate_types(gc)
  generate(AST.definitions, gc)
  add_predefined_names(gc)
  generate_header(gc)
  generate(AST.main, gc)
  generate_footer(gc)
  ptln(gc, "")
end

function add_predefined_names(gc::GenerationContext)
  # not implemented yet
end

function generate(d::Definitions, gc::GenerationContext)
  for subroutine in d.defs
    generate(subroutine, gc)
  end
end

function generate(s::Subroutine, gc::GenerationContext)
  ret_type = s.ret_type.var_type
  llvm_ret_type = mptype_to_llvm_type[ret_type]
  params = s.params.params
  println("this is generate subr, params is $params")
  args = Vector{String}()
  for param::Parameter in params
    true_type = (param.is_var_par || param.is_array) ? scalar_to_ref_type[param.var_type] : param.var_type
    pushvar!(gc, param.name, param.var_type, "%$(param.name)")
    push!(args, "$(mptype_to_llvm_type[true_type]) %$(param.name)")
  end
  argstring = join(args, ", ")
  ptln(gc, "define $llvm_ret_type @$(s.name.lexeme)($argstring) {")
  generate(s.body, gc)
  if ret_type == MNothing
    ptln(gc, "ret void", 1)
  end
  ptln(gc, "}")
  pushsubroutine!(gc, s.name.lexeme, Vector{MPType}(), s.ret_type.var_type)
end

function generate(b::Block, gc::GenerationContext)
  enterscope!(gc)
  for statement in b.statements
    generate(statement, gc)
  end
  popscope!(gc)
end

function generate(d::Declaration, gc::GenerationContext)
  llvm_type = mptype_to_llvm_type[d.var_type.var_type]
  for name in d.names
    llvm_name = create_id(gc, "alloc")
    ptln(gc, "$llvm_name = alloca $llvm_type", 1)
    pushvar!(gc, name.lexeme, d.var_type.var_type, llvm_name)
  end
end

function generate(i::IfThen, gc::GenerationContext)
  cond_id = generate(i.condition, gc)
  then_label = create_id(gc, "then")
  end_label = create_id(gc, "end_if")
  ptln(gc, "br i1 $cond_id, label $then_label, label $end_label", 1)
  ptln(gc, "$(then_label[2:end]):")
  cond_id = generate(i.then_stmt, gc)
  ptln(gc, "br label $end_label", 1)
  ptln(gc, "$(end_label[2:end]):")
end

function generate(i::IfThenElse, gc::GenerationContext)
  cond_id = generate(i.condition, gc)
  then_label = create_id(gc, "then_")
  else_label = create_id(gc, "else_")
  end_label = create_id(gc, "end_if_")
  ptln(gc, "br i1 $cond_id, label $then_label, label $else_label", 1)
  ptln(gc, "$(then_label[2:end]):")
  generate(i.then_stmt, gc)
  ptln(gc, "br label $end_label", 1)
  ptln(gc, "$(else_label[2:end]):")
  generate(i.else_stmt, gc)
  ptln(gc, "br label $end_label", 1)
  ptln(gc, "$(end_label[2:end]):")
end

function generate(w::While, gc::GenerationContext)
  while_label = create_id(gc, "while_")
  do_label = create_id(gc, "do_")
  end_label = create_id(gc, "end_if_")
  ptln(gc, "br label $while_label", 1)
  ptln(gc, "$(while_label[2:end]):")
  cond_id = generate(w.condition, gc)
  ptln(gc, "br i1 $cond_id, label $do_label, label $end_label", 1)
  ptln(gc, "$(do_label[2:end]):")
  generate(w.do_stmt, gc)
  ptln(gc, "br label $while_label", 1)
  ptln(gc, "$(end_label[2:end]):")
end

function generate(c::CallStatement, gc::GenerationContext)
  subroutine::SubroutineEntry = getsubroutine(gc, c.identifier.lexeme)
  println("this is generate CallStatement, args are $(c.arguments)")
  arg_ids = [generate(arg, gc) for arg in c.arguments]
  arg_types = [mptype_to_llvm_type[arg.type] for arg in c.arguments]
  arg_strs = ["$type $id" for (type, id) in zip(arg_types, arg_ids)]
  arg_string = join(arg_strs,  ", ")
  ptln(gc, "call void @$(subroutine.name)($arg_string)", 1)
end

function generate(r::Return, gc::GenerationContext)
  val_id = generate(r.value, gc)
  llvm_ret_type = mptype_to_llvm_type[r.value.type]
  ptln(gc, "ret $llvm_ret_type $val_id", 1)
end

function generate(a::Assignment, gc::GenerationContext)
  value_llvm_type = mptype_to_llvm_type[a.value.type]
  value_id = generate(a.value, gc)
  entry::StackEntry = getvariable(gc, a.variable.lexeme)
  variable_pointer = entry.val_identifier
  ptln(gc, "store $value_llvm_type $value_id, $value_llvm_type* $variable_pointer", 1)
end

function generate(p::Write, gc::GenerationContext)
  for arg in p.arguments
    arg_id = generate(arg, gc)
    if arg.type == MBool
      ptln(gc, "call void @print_bool(i1 $(arg_id))", 1)
    elseif arg.type ∈ [MInt, MReal]
      arg_type = mptype_to_llvm_type[arg.type]
      envelope_id = arg.type == MInt ? INT_STR_ID : REAL_STR_ID
      pt(gc, "call i32 (i8*, ...) ", 1)
      ptln(gc, "@printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* $(envelope_id), i32 0, i32 0), $(arg_type) $(arg_id))")
    elseif arg.type == MString
      str_ptr_id = create_id(gc, "str_ptr")
      ptln(gc, "$str_ptr_id = getelementptr $STRING_TYPE_ID, $STRING_TYPE_ID* $arg_id, i32 0, i32 0", 1)
      str_id = create_id(gc, "str")
      ptln(gc, "$str_id = load i8*, i8** $str_ptr_id", 1)
      ptln(gc, "call i32 (i8*, ...) @printf(i8* getelementptr ([3 x i8], [3 x i8]* $STR_STR_ID, i32 0, i32 0), i8* $str_id)", 1)
    end
  end
  ptln(gc, "call i32 @puts(i8* getelementptr inbounds ([1 x i8], [1 x i8]* $EMPTY_STR_ID, i32 0, i32 0))", 1)
end

function generate(v::VarAsPar, gc::GenerationContext)
  variable::StackEntry = getvariable(gc, v.name)
  println("this is generate VarAsPar, variable is $variable")
  return variable.val_identifier
end

function generate(s::SimpleExpression, gc::GenerationContext)
  ret_type = s.terms[1][1].type
  llvm_type = mptype_to_llvm_type[ret_type]
  ret_id = generate(s.terms[1][1], gc)
  if ret_type == MInt
    signed_id = create_id(gc, "signed_int")
    sign_op = (s.terms[1][2].class == plus ? "add" : "sub")
    ptln(gc, "$(signed_id) = $(sign_op) i64 0, $(ret_id)", 1)
    ret_id = signed_id
  elseif ret_type == MReal
    signed_id = create_id(gc, "signed_real")
    sign_op = (s.terms[1][2].class == plus ? "fadd" : "fsub")
    ptln(gc, "$(signed_id) = $(sign_op) double 0.0, $(ret_id)", 1)
    ret_id = signed_id
  end
  if length(s.terms) > 1
    for (term, op) in s.terms[2:end]
      term_id = generate(term, gc)
      if ret_type == MString
        new_ret_id = create_id(gc, "concat")
        length_id = create_id(gc, "str_len")
        new_str_id = create_id(gc, "new_str")
        ptln(gc, "$new_ret_id = alloca $STRING_TYPE_ID", 1)
        ptln(gc, "$length_id = call i32 @combined_length($STRING_TYPE_ID* $ret_id, $STRING_TYPE_ID* $term_id)", 1)
        ptln(gc, "$new_str_id = alloca i8, i32 $length_id", 1)
        ptln(gc, "call void $CONCAT_ID($STRING_TYPE_ID* $new_ret_id, i8* $new_str_id, $STRING_TYPE_ID* $ret_id, $STRING_TYPE_ID* $term_id)", 1)
        ret_id = new_ret_id
      else
        new_ret_id = create_id(gc, "sum_diff_or")
        opcode = add_op_and_type_to_opcode[(op.class, ret_type)]
        ptln(gc, "$(new_ret_id) = $(opcode) $llvm_type $(ret_id), $(term_id)", 1)
        ret_id = new_ret_id
      end
    end
  end
  return ret_id
end

function generate(r::RelationalExpression, gc::GenerationContext)
  arg_type = r.left.type
  arg_llvm_type = mptype_to_llvm_type[arg_type]
  left = generate(r.left, gc)
  right = generate(r.right, gc)
  ret_id = create_id(gc, "cond")
  if arg_type ∈ [MInt, MReal, MBool]
    cond = op_and_type_to_cond[(r.operation.class, arg_type)]
    opcode = arg_type == MReal ? "fcmp" : "icmp"
    ptln(gc, "$ret_id = $opcode $cond $arg_llvm_type $left, $right", 1)
  else
    comp_id = create_id(gc, "comp")
    ptln(gc, "$comp_id = call i32 $COMP_STRS_ID($STRING_TYPE_ID* $left, $STRING_TYPE_ID* $right)", 1)
    cond = op_and_type_to_cond[(r.operation.class, MInt)]
    ptln(gc, "$ret_id = icmp $cond i32 $comp_id, 0", 1)
  end
  return ret_id
end

function generate(t::Term, gc::GenerationContext)
  ret_type = t.factors[1][1].type
  llvm_type = mptype_to_llvm_type[ret_type]
  first_factor_id = generate(t.factors[1][1], gc)
  ret_id = first_factor_id
  if length(t.factors) > 1
    for (factor, op) in t.factors[2:end]
      fact_id = generate(factor, gc)
      new_ret_id = create_id(gc, "prod_quot_mod")
      opcode = mult_op_and_type_to_opcode[(op.class,ret_type)]
      ptln(gc, "$(new_ret_id) = $(opcode) $(llvm_type) $(ret_id), $(fact_id)", 1)
      ret_id = new_ret_id
    end
  end
  return ret_id
end

function generate(l::LiteralFactor, gc::GenerationContext)
  ret_id = ""
  if l.type == MInt
    ret_id = create_id(gc, "int_lit")
    ptln(gc, "$(ret_id) = add i64 0, $(l.token.lexeme) ", 1)
  elseif l.type == MBool
    ret_id = create_id(gc, "bool_lit")
    if l.token.class == kw_true
      ptln(gc, "$(ret_id) = or i1 1, 1 ", 1)
    else
      ptln(gc, "$(ret_id) = and i1 0, 0 ", 1)
    end
  elseif l.type == MReal
    ret_id = create_id(gc, "real_lit")
    ptln(gc, "$(ret_id) = fadd double 0.0, $(l.token.lexeme) ", 1)
  elseif l.type == MString
    ret_id = create_id(gc, "str_lit")
    str_len = length(l.token.lexeme) + 1  # Account for terminating null
    ptln(gc, "$ret_id = alloca $STRING_TYPE_ID", 1)
    str_ptr_id = create_id(gc, "str_ptr")
    lit_as_ptr_id = create_id(gc, "lit_ptr")
    ptln(gc, "$lit_as_ptr_id = bitcast [$str_len x i8]* $(l.unique_id) to i8*", 1)
    ptln(gc, "$str_ptr_id = getelementptr $STRING_TYPE_ID, $STRING_TYPE_ID* $ret_id, i32 0, i32 0", 1)
    ptln(gc, "store i8* $lit_as_ptr_id, i8** $str_ptr_id", 1)
    str_len_ptr_id = create_id(gc, "str_len")
    ptln(gc, "$str_len_ptr_id = getelementptr $STRING_TYPE_ID, $STRING_TYPE_ID* $ret_id, i32 0, i32 1", 1)
    ptln(gc, "store i32 $(str_len), i32* $str_len_ptr_id", 1)
  else
    println("codegen for non-int literals not implemented yet")
  end
  return ret_id
end

function generate(p::ParenFactor, gc::GenerationContext)
  return generate(p.expression, gc)
end

function generate(v::VariableFactor, gc::GenerationContext)
  llvm_type = mptype_to_llvm_type[v.type]
  entry = getvariable(gc, v.identifier.lexeme)
  ret_id = create_id(gc, "var_val")
  ptln(gc, "$ret_id = load $llvm_type, $llvm_type* $(entry.val_identifier)", 1)
  return ret_id
end

function generate(n::NotFactor, gc::GenerationContext)
  arg_id = generate(n.argument, gc)
  ret_id = create_id(gc, "not")
  ptln(gc, "$ret_id = add i1 $arg_id, 1", 1)
  return ret_id
end

function generate(c::CallFactor, gc::GenerationContext)
  println("this is generate CallFactor, sr id lex is $(c.identifier.lexeme)")
  subroutine::SubroutineEntry = getsubroutine(gc, c.identifier.lexeme)
  arg_ids = [generate(arg, gc) for arg in c.arguments]
  arg_types = [mptype_to_llvm_type[arg.type] for arg in c.arguments ]
  arg_strs = ["$type $id" for (type, id) in zip(arg_types, arg_ids)]
  arg_string = join(arg_strs,  ", ")
  ret_id = create_id(gc, "ret")
  llvm_ret_type = mptype_to_llvm_type[subroutine.return_type]
  ptln(gc, "$ret_id = call $llvm_ret_type @$(subroutine.name)($arg_string)", 1)
  return ret_id
end
