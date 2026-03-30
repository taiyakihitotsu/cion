import type Cion from '../../src/index'
import type { Str, Eval } from '../../src/index'
import type { Equal } from '../../src/util'

// Internal Eval and Str type tests
const str_internal_test_0 : true = {} as Equal<[`prim`, "'head/tail'"], Eval<[[`sym`, `str`], [`prim`, `head/`], [`prim`, `tail`]]>>

const str_internal_test_1 : true = {} as Equal<[`prim`, "'test+tail'"], Str<[[`prim`, `test`], [`prim`, `+`], [`prim`, `tail`]]>>
const str_internal_test_2 : true = {} as Equal<[`prim`, "'test'"],      Str<[[`prim`, `test`]]>>

// Lisp (string result) tests
const str_lisp_test_0 : true = {} as Equal<"'ab'",     Cion.Lisp<`(str 'a' 'b')`>>
const str_lisp_test_1 : true = {} as Equal<"'012'",    Cion.Lisp<`(str 0 1 2)`>>
const str_lisp_test_2 : true = {} as Equal<"'012'",    Cion.Lisp<`(str 0 1/1 2)`>>
const str_lisp_test_3 : true = {} as Equal<"'02/32'",  Cion.Lisp<`(str 0 2/3 2)`>>
const str_lisp_test_4 : true = {} as Equal<"'0nil2'",   Cion.Lisp<`(str 0 nil 2)`>>
const str_lisp_test_5 : true = {} as Equal<"'0false2'", Cion.Lisp<`(str 0 false 2)`>>

// Collection serialization tests
const str_coll_test_0 : true = {} as Equal<"'0[]2'",           Cion.Lisp<`(str 0 [] 2)`>>
const str_coll_test_1 : true = {} as Equal<"'0nil2'",          Cion.Lisp<`(str 0 {} 2)`>> // Note: Empty map as nil in this context
const str_coll_test_2 : true = {} as Equal<"'0[0 1]2'",        Cion.Lisp<`(str 0 [0 1] 2)`>>
const str_coll_test_3 : true = {} as Equal<"'0{:a 0}2'",       Cion.Lisp<`(str 0 {:a 0} 2)`>>
const str_coll_test_4 : true = {} as Equal<"'0[0 1]{:a 0}2'",  Cion.Lisp<`(str 0 [0 1] {:a 0} 2)`>>
