import type Cion from '../../src/index'
import type { Equal } from '../../src/util'

const replace_test_0 : true = {} as Equal<"'aaaaaaaaa'", Cion.Lisp<`(replace 'ababababa' 'b' 'a')`>>
const replace_test_1 : true = {} as Equal<"'abxxxxba'",  Cion.Lisp<`(replace 'ab5468ba' '[\\d]' 'x')`>>
const replace_test_2 : true = {} as Equal<"'ababababa'", Cion.Lisp<`(replace 'ababababa' 'x' 'a')`>>

// Replacement with functions
const replace_fn_test_0 : true = {} as Equal<"'aaaaaaaaa'",     Cion.Lisp<`(replace 'ababababa' 'b' (fn [_ y _] 'a'))`>>
const replace_fn_test_1 : true = {} as Equal<"'abbabbabbabba'", Cion.Lisp<`(replace 'ababababa' 'b' (fn [_ y _] (str y y)))`>>

// Function arguments test (x: matched, y: match-all/context, z: remainder)
const replace_arg_test_0 : true = {} as Equal<"'aacdefg'",     Cion.Lisp<`(replace 'abcdefg' 'b' (fn [x y z] x))`>>
const replace_arg_test_1 : true = {} as Equal<"'abcdefg'",     Cion.Lisp<`(replace 'abcdefg' 'b' (fn [x y z] y))`>>
const replace_arg_test_2 : true = {} as Equal<"'acdefgcdefg'", Cion.Lisp<`(replace 'abcdefg' 'b' (fn [x y z] z))`>>

// Regex match with function
const replace_regex_fn_test_0 : true = {} as Equal<"'aa'",               Cion.Lisp<`(replace 'abcdefg' '[b-g]' (fn [x y z] x))`>>
const replace_regex_fn_test_1 : true = {} as Equal<"'abcdefg'",          Cion.Lisp<`(replace 'abcdefg' '[b-g]' (fn [x y z] y))`>>
const replace_regex_fn_test_2 : true = {} as Equal<"'acdefgdefgefgfgg'", Cion.Lisp<`(replace 'abcdefg' '[b-g]' (fn [x y z] z))`>>

// Edge cases and final checks
const replace_misc_test_0 : true = {} as Equal<"' '",    Cion.Lisp<`((fn [x y z] z) '' '' ' ')`>>
const replace_misc_test_1 : true = {} as Equal<"'aaaaaa'", Cion.Lisp<`(replace 'aabbaa' 'b' 'a')`>>
