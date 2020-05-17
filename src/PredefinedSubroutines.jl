const INT_STR_ID = "@int_str"
const REAL_STR_ID = "@real_str"
const STR_STR_ID = "@str_str"
const TRUE_STR_ID = "@true"
const FALSE_STR_ID = "@false"
const EMPTY_STR_ID = "@empty"

const STRING_TYPE_ID = "%string"

const CONCAT_ID = "@concat"
const COMB_LEN_ID = "@combined_length"
const WRITE_BOOL_ID = "@print_bool"

const write_bool = """
define void $WRITE_BOOL_ID(i1 %a) {
entry:
  br i1 %a, label %is_true, label %is_false
is_true:
  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* $(TRUE_STR_ID), i32 0, i32 0), i1 %a)
  br label %end
is_false:
  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* $(FALSE_STR_ID), i32 0, i32 0), i1 %a)
  br label %end
end:
  ret void
}
"""

const combined_length = """
define i32 $COMB_LEN_ID($STRING_TYPE_ID* %str1, $STRING_TYPE_ID* %str2) {
  %ptr1 = getelementptr $STRING_TYPE_ID, $STRING_TYPE_ID* %str1, i32 0, i32 1
  %ptr2 = getelementptr $STRING_TYPE_ID, $STRING_TYPE_ID* %str2, i32 0, i32 1
  %X.0 = load i32, i32* %ptr1
  %X.1 = load i32, i32* %ptr2
  %X.2 = add i32 %X.0, %X.1
  %X.3 = add i32 %X.2, 1
  ret i32 %X.3
}
"""

const concat = """
define void $CONCAT_ID($STRING_TYPE_ID* %dest, i8* %new_string, $STRING_TYPE_ID* %head, $STRING_TYPE_ID* %tail) {
  %dest.str.ptr = getelementptr $STRING_TYPE_ID, $STRING_TYPE_ID* %dest, i32 0, i32 0
  store i8* %new_string, i8** %dest.str.ptr
  %head.str.ptr = getelementptr $STRING_TYPE_ID, $STRING_TYPE_ID* %head, i32 0, i32 0
  %head.str = load i8*, i8** %head.str.ptr
  %tail.str.ptr = getelementptr $STRING_TYPE_ID, $STRING_TYPE_ID* %tail, i32 0, i32 0
  %tail.str = load i8*, i8** %tail.str.ptr
  call i8* @strcpy(i8* %new_string, i8* %head.str)
  call i8* @strcat(i8* %new_string, i8* %tail.str)
  ret void
}
"""


const HELPER_FUNCTIONS = [
  write_bool,
  combined_length,
  concat,
]