include("StaticAnalyzer.jl")
include("PredefinedSubroutines.jl")

binary_op_and_type_to_opcode = Dict{Tuple{TokenClass,MPType},String}(
  (plus, MInt) => "add",
  (plus, MReal) => "fadd",
  (minus, MInt) => "sub",
  (minus, MReal) => "fsub",
  (kw_or, MBool) => "or",
  (times, MInt) => "mul",
  (times, MReal) => "fmul",
  (divide, MInt) => "sdiv",
  (divide, MReal) => "fdiv",
  (modulo, MInt) => "srem",
  (kw_and, MBool) => "and",

  (equals, MInt) => "icmp eq",
  (equals, MBool) => "icmp eq",
  (equals, MReal) => "fcmp oeq",
  (not_equal, MInt) => "icmp ne",
  (not_equal, MBool) => "icmp ne",
  (not_equal, MReal) => "fcmp one",
  (less_than, MInt) => "icmp slt",
  (less_than, MBool) => "icmp ult",
  (less_than, MReal) => "fcmp olt",
  (less_than_or_equal, MInt) => "icmp sle",
  (less_than_or_equal, MBool) => "icmp ule",
  (less_than_or_equal, MReal) => "fcmp ole",
  (greater_than, MInt) => "icmp sgt",
  (greater_than, MBool) => "icmp ugt",
  (greater_than, MReal) => "fcmp ogt",
  (greater_than_or_equal, MInt) => "icmp sge",
  (greater_than_or_equal, MBool) => "icmp uge",
  (greater_than_or_equal, MReal) => "fcmp oge",
)

unary_op_and_type_to_opcode_and_first_arg = Dict{Tuple{TokenClass,MPType},Tuple{String,String}}(
  (kw_not, MBool) => ("add", "1"),
  (minus, MInt) => ("mul", "-1"),
  (minus, MReal) => ("fmul", "-1.0"),
)

mptype_to_llvm_type = Dict{MPType,String}(
  MInt => "i32",
  MIntRef => "i32*",
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
  subroutines::Vector{SubroutineEntry}
  generated_subrs::Dict{String,String}
  calls::Vector{Call}
  string_literals::Vector{LiteralFactor}
  all_var_names_and_types::Vector{Tuple{String,MPType}}
  io
end

function GenerationContext(
    subroutines::Vector{SubroutineEntry},
    calls::Vector{Call},
    string_literals::Vector{LiteralFactor},
    io)
    GenerationContext(0, subroutines, Dict{String,String}(), calls, string_literals, Vector{Tuple{String,MPType}}(), io)
  end

function create_id(gc::GenerationContext, str::String)
  id_string = "%$(str)_$(gc.counter)"
  gc.counter += 1
  return id_string
end

function reset_id_counter(gc::GenerationContext)
  gc.counter = 0
end

function pt(gc::GenerationContext, str, indent)
  print(gc.io, '\t'^indent * str)
end

pt(gc, str) = pt(gc, str, 0)
ptln(gc, str, indent) = pt(gc, str * '\n', indent)
ptln(gc, str) = ptln(gc, str, 0)

function generate_header(gc::GenerationContext)
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

function generate_subroutines(gc::GenerationContext)
  for entry::SubroutineEntry in gc.subroutines
    subroutine::Subroutine = entry.node
    generate(subroutine, gc)
  end
end

function generate(s::Subroutine, gc::GenerationContext)
  # for fingerprint in keys(s.fingerprint_to_implicits_and_f_id)
  #   implicits, llvm_function_name = s.fingerprint_to_implicits_and_f_id[fingerprint]
  #   generate_subroutine(s, implicits, llvm_function_name, gc)
  # end
  if s.is_called
    generate_subroutine(s, s.llvm_function_name, gc)
  end
end

function generate_subroutine(s::Subroutine, llvm_function_name::String, gc::GenerationContext)
  println("This is generate_subroutine, subr name is $(s.name)")
  println("llvm function name is $(llvm_function_name)")
  llvm_ret_type = mptype_to_llvm_type[s.ret_type.scalar_type]
  args = ["$(mptype_to_llvm_type[parameter_to_mptype(param)]) %$(param.name)" for param in s.params]
  for (name, mptype) in gc.all_var_names_and_types
    as_reftype = scalar_to_ref_type[mptype]
    name_as_implicit = get_implicit_param_id(name, mptype)
    push!(args, "$(mptype_to_llvm_type[as_reftype]) %$(name_as_implicit)")
  end
  argstring = join(args, ", ")
  signature_string = "$llvm_ret_type $(s.llvm_function_name)($argstring)"
  ptln(gc, "define $signature_string {")
  generate(s.body, gc)
  if s.ret_type.scalar_type == MNothing  # Subroutine is a procedure
    ptln(gc, "ret void", 1)
  end
  ptln(gc, "}")
  ptln(gc, "")
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
  ptln(gc, "$ARRAY_TYPE_ID = type { i8*, i32 }")
  ptln(gc, "")
end

function generate_helpers(gc)
  for helper in HELPER_FUNCTIONS
    ptln(gc, helper)
  end
end

generate(program::String) = generate(program::String, IOBuffer())

function generate(program::String, io)
  token_sequence = scan_input(program)
  DEBUG && println("scanning finished")
  pc = ParsingContext(token_sequence)
  AST::Program = parse_input(token_sequence, pc)
  DEBUG && println("parsing finished")
  ac = AnalysisContext()
  ac.all_var_names_and_types = pc.all_var_names_and_types
  static_analysis(AST, ac)
  DEBUG && println("static analysis finished")
  # DEBUG && println("subroutines vector contains ", ac.subroutines)
  # DEBUG && println("unique calls contain ", ac.unique_calls)
  gc = GenerationContext(ac.subroutines, ac.unique_calls, pc.string_literals, io)
  gc.all_var_names_and_types = pc.all_var_names_and_types
  generate(AST, gc)
  DEBUG && println("codegen finished")
  reset_id_counter(gc)
end

function generate(AST::Program, gc::GenerationContext)
  st = AnalysisContext()
  generate_types(gc)
  generate_default_values(gc)
  generate_strings(gc)
  generate_subroutines(gc)
  add_predefined_names(gc)
  generate_header(gc)
  generate(AST.main, gc)
  generate_footer(gc)
end

function generate_default_values(gc::GenerationContext)
  all_types = Vector{MPType}(unique(type for (name, type) in gc.all_var_names_and_types))
  # TODO: exclude arrays etc. if necessary
  for type in setdiff(all_types, [MString])
    ptln(gc, "@default_$(type) = global $(mptype_to_llvm_type[type]) $(default_value_strings[type])")
  end
end

function add_predefined_names(gc::GenerationContext)
  # not implemented yet
end

function generate(b::Block, gc::GenerationContext)
  for statement in b.statements
    generate(statement, gc)
  end
end

function generate(d::Declaration, gc)
  mptype = d.var_type.true_type
  llvm_type = mptype_to_llvm_type[mptype]
  for id in d.unique_ids
    ptln(gc, "$id = alloca $(llvm_type)", 1)
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
  generate_call(c, gc)
  return
end

function generate(c::CallFactor, gc::GenerationContext)
  generate_call(c, gc)
end

function generate_call(c::Call, gc::GenerationContext)
  println("This is generate_call. Call is to $(c.identifier)")
  println("This is generate_call. Args are $(c.arguments)")
  subroutine::Subroutine = c.subroutine
  ret_type = subroutine.ret_type.true_type
  
  explicit_arg_ids = [generate(arg, gc) for arg in c.arguments]
  explicit_arg_types = [mptype_to_llvm_type[arg.type] for arg in c.arguments ]

  llvm_function_name = subroutine.llvm_function_name
  println("This is generate_call. Implicit params are $(c.implicit_params)")

  # implicit_arg_ids = [param.val_identifier for param in c.implicit_params]
  # implicit_arg_types = [mptype_to_llvm_type[scalar_to_ref_type[param.var_type]] for param in c.implicit_params]

  implicit_arg_ids, implicit_arg_types = get_implicit_ids_and_llvm_types(c.implicit_params, gc)

  arg_ids = [explicit_arg_ids; implicit_arg_ids]
  arg_types = [explicit_arg_types; implicit_arg_types]

  arg_strs = ["$type $id" for (type, id) in zip(arg_types, arg_ids)]
  arg_string = join(arg_strs,  ", ")

  ret_id = create_id(gc, "ret")
  llvm_ret_type = mptype_to_llvm_type[ret_type]
  if ret_type == MNothing  # Subroutine is a procedure
    ptln(gc, "call $llvm_ret_type $(llvm_function_name)($arg_string)", 1)
  else
    ptln(gc, "$ret_id = call $llvm_ret_type $(llvm_function_name)($arg_string)", 1)
  end
  return ret_id
end

function get_implicit_ids_and_llvm_types(entries::Vector{SymTableEntry}, gc::GenerationContext)
  ids = []
  types = []
  for entry in entries
    if entry.var_type ∈ ref_types
      push!(ids, entry.val_identifier)
      push!(types, entry.var_type)
    else
      push!(ids, get_ref(entry.val_identifier, entry.var_type, gc))
      push!(types, entry.var_type)
    end
  end
  types = map(t->mptype_to_llvm_type[scalar_to_ref_type[t]], types)
  return ids, types
end

function get_ref(id::String, mptype::MPType, gc::GenerationContext)
  ref_id = create_id(gc, "ref_$(mptype)")
  llvm_type = mptype_to_llvm_type[mptype]
  ptln(gc, "$ref_id = alloca $llvm_type", 1)
  return ref_id
end

function generate(r::Return, gc::GenerationContext)
  val_id = generate(r.value, gc)
  llvm_ret_type = mptype_to_llvm_type[r.value.type]
  ptln(gc, "ret $llvm_ret_type $val_id", 1)
end

function generate(a::Assignment, gc::GenerationContext)
  var_type = a.var_type
  value_llvm_type = mptype_to_llvm_type[a.value.type]
  value_id = generate(a.value, gc)
  variable_pointer = a.var_unique_id
  if var_type ∈ keys(ref_type_to_scalar_type)
    ptln(gc, "store $value_llvm_type $value_id, $value_llvm_type* $variable_pointer", 1)
  else
    ptln(gc, "$(a.var_unique_id) = $(get_identity(var_type)) $(value_id)", 1)
  end
end

function get_identity(type::MPType)
  if type == MInt
    return "add i32 0,"
  elseif type == MReal
    return "fadd double 0.0,"
  elseif type == MBool
    return "add i1 0,"
  elseif type == MString
    println("identity function for strings not implemented yet")
    return
  end
  println("identity function for $(type) not implemented yet")
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

function generate(b::BinaryOperation, gc::GenerationContext)
  ret_llvm_type = mptype_to_llvm_type[b.type]
  ret_id = create_id(gc, "$(b.type)")
  left = generate(b.left, gc)
  right = generate(b.right, gc)
  if b.left.type == MString return generate_string_op(b.op, left, right, gc) end
  operand_llvm_type = mptype_to_llvm_type[b.left.type]
  opcode = binary_op_and_type_to_opcode[(b.op, b.left.type)]
  ptln(gc, "$ret_id = $opcode $operand_llvm_type $left, $right", 1)
  return ret_id
end

function generate(u::UnaryOperation, gc::GenerationContext)
  ret_llvm_type = mptype_to_llvm_type[u.type]
  ret_id = create_id(gc, "$(u.type)")
  operand = generate(u.operand, gc)
  operand_llvm_type = mptype_to_llvm_type[u.operand.type]
  opcode, first_arg = unary_op_and_type_to_opcode_and_first_arg[(u.op, u.operand.type)]
  ptln(gc, "$ret_id = $opcode $operand_llvm_type $operand, $first_arg", 1)
  return ret_id
end

function generate(g::GetRef, gc::GenerationContext)
  return g.operand.variable_entry.val_identifier
end

function generate_string_op(op::TokenClass, left::String, right::String, gc::GenerationContext)
  if op == plus
    return concat_strings(left, right, gc)
  else
    return string_comparison(op, left, right, gc)
  end
end

function concat_strings(left::String, right::String, gc::GenerationContext)
  ret_id = create_id(gc, "concat")
  length_id = create_id(gc, "str_len")
  new_str_id = create_id(gc, "new_str")
  ptln(gc, "$ret_id = alloca $STRING_TYPE_ID", 1)
  ptln(gc, "$length_id = call i32 @combined_length($STRING_TYPE_ID* $left, $STRING_TYPE_ID* $right)", 1)
  ptln(gc, "$new_str_id = alloca i8, i32 $length_id", 1)
  ptln(gc, "call void $CONCAT_ID($STRING_TYPE_ID* $ret_id, i8* $new_str_id, $STRING_TYPE_ID* $left, $STRING_TYPE_ID* $right)", 1)
  return ret_id
end

function string_comparison(op::TokenClass, left::String, right::String, gc::GenerationContext)
  ret_id = create_id(gc, "str_cmp")
  comp_tmp_id = create_id(gc, "comp_tmp")
  ptln(gc, "$comp_tmp_id = call i32 $COMP_STRS_ID($STRING_TYPE_ID* $left, $STRING_TYPE_ID* $right)", 1)
  cond = op_and_type_to_cond[(op, MInt)]
  ptln(gc, "$ret_id = icmp $cond i32 $comp_tmp_id, 0", 1)
  return ret_id
end

function generate(l::LiteralFactor, gc::GenerationContext)
  ret_id = ""
  if l.type ∈ [MInt, MReal, MBool]
    return l.token.lexeme
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
    println("codegen for arrays not implemented")
  end
  return ret_id
end

function generate(p::ParenFactor, gc::GenerationContext)
  return generate(p.expression, gc)
end

function generate(v::VariableFactor, gc::GenerationContext)
  llvm_type = mptype_to_llvm_type[v.type]
  entry::SymTableEntry = v.variable_entry
  println("this is generate VariableFactor, val_identifier is ", entry.val_identifier)
  println("variable type is $(v.type)")
  println("entry type is $(v.variable_entry.var_type)")
  println("val_identifier is $(entry.val_identifier)")
  ret_id = create_id(gc, "var_val")
  if v.variable_entry.var_type ∉ ref_types
    return entry.val_identifier
  else
    ptln(gc, "$ret_id = load $llvm_type, $llvm_type* $(entry.val_identifier)", 1)
  end
  return ret_id
end

function generate(a::ArrayAccessFactor, gc::GenerationContext)
  variable_entry::SymTableEntry = getvariable(gc, a.identifier.lexeme)
  val_id = variable_entry.val_identifier
  scalar_type = ref_type_to_scalar_type[variable_entry.var_type]
  llvm_type = mptype_to_llvm_type[scalar_type]
  index_id = generate(a.index, gc)
  arr_ptr_id = create_id(gc, "arr_ptr")
  val_ptr_id = create_id(gc, "val_ptr")
  ret_id = create_id(gc, "array_el")
  len_id = create_id(gc, "array_len_ptr")
  ptln(gc, "$len_id = call i32 $GET_LENGTH_ID($ARRAY_TYPE_ID* $val_id)", 1)
  # TODO: runtime array bounds checking
  ptln(gc, "$arr_ptr_id = call i8* $GET_ARRAY_PTR_ID($ARRAY_TYPE_ID* $val_id)", 1)
  ptln(gc, "$val_ptr_id = getelementptr $llvm_type, $llvm_type* bitcast (i8* $arr_ptr_id to $llvm_type), i32 0, i32 $index_id", 1)
  ptln(gc, "$ret_id = load $llvm_type, $llvm_type* $val_ptr_id", 1)
end

function generate(n::NotFactor, gc::GenerationContext)
  arg_id = generate(n.argument, gc)
  ret_id = create_id(gc, "not")
  ptln(gc, "$ret_id = add i1 $arg_id, 1", 1)
  return ret_id
end

function get_subroutine_fingerprint(s::Subroutine, implicits::Vector{SymTableEntry})
  explicits = ["$(parameter_to_llvmtype(p)) $(p.name)" for p::Parameter in s.params]
  implicits = ["$(mptype_to_llvm_type[s.var_type]) $(s.var_name)" for s::SymTableEntry in implicits]
  "$(s.name)/$(join(explicits, ", "))/$(join(implicits, ", "))"
end

