import type { Cion } from '../../src/index'
import type { Equal } from '../../src/util'

// Basic some->> behavior
const some_last_arrow_test_0 : true = {} as Equal<'true',  Cion.Lisp<`(some->> 1 number?)`>>
const some_last_arrow_test_1 : true = {} as Equal<'false', Cion.Lisp<`(some->> 1 number? nil?)`>>
const some_last_arrow_test_2 : true = {} as Equal<'true',  Cion.Lisp<`(some->> 1 number?)`>>

// Short-circuiting and logical flow
const some_last_arrow_nil_test : true = {} as Equal<'nil', Cion.Lisp<`(some->> 1 number? (-> nil? not) string?)`>>
const some_last_arrow_str_test : true = {} as Equal<'false', Cion.Lisp<`(string? (-> nil? not true))`>>

// Intermediate calculations
const some_last_arrow_calc_test : true = {} as Equal<'true', Cion.Lisp<`(some->> 1 (+ 2 2) number?)`>>

// False propagation (not nil)
const some_last_arrow_false_test_0 : true = {} as Equal<'false', Cion.Lisp<`(some->> 1 boolean? number?)`>>
const some_last_arrow_false_test_1 : true = {} as Equal<'2',     Cion.Lisp<`(some->> 1 boolean? ((fn [n] (if (= n false) 2 4))))`>>
const some_last_arrow_false_test_2 : true = {} as Equal<'true',  Cion.Lisp<`(some->> 1 boolean? ((fn [n] (if (= n false) 2 4))) number?)`>>

// @ts-expect-error:
const testsometh2: Cion.Lisp<`(some->> 1 number? (-> nil? not))`> = 'nil'
