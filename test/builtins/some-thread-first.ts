import type Cion from '../../src/index'
import type { Equal } from '../../src/util'

// Basic some-> behavior
const some_arrow_test_0 : true = {} as Equal<'true', Cion.Lisp<`(some-> 1 number?)`>>
const some_arrow_test_1 : true = {} as Equal<'false', Cion.Lisp<`(some-> 1 number? nil?)`>>

// Short-circuiting with nil
const some_arrow_nil_test_0 : true = {} as Equal<'false', Cion.Lisp<`(some-> 1 string? nil?)`>>
const some_arrow_nil_test_1 : true = {} as Equal<'false', Cion.Lisp<`(some-> 1 string? string?)`>>
const some_arrow_nil_test_2 : true = {} as Equal<'nil',   Cion.Lisp<`(some-> [] first)`>>
const some_arrow_nil_test_3 : true = {} as Equal<'nil',   Cion.Lisp<`(some-> [] first string?)`>>
const some_arrow_nil_test_4 : true = {} as Equal<'nil',   Cion.Lisp<`(some-> 1 (fn [n] nil) string?)`>>
const some_arrow_nil_test_5 : true = {} as Equal<'nil',   Cion.Lisp<`(some-> 1 number? (fn [n] nil) string?)`>>

// Truthiness and nil? checks
const nil_check_test_0 : true = {} as Equal<'false', Cion.Lisp<`(if (nil? (if (number? 1) (number? 1) false)) (nil? (if (number? 1) (number? 1) false)) false)`>>
const nil_check_test_1 : true = {} as Equal<'false', Cion.Lisp<`(nil? (if (number? 1) (number? 1) false))`>>
const nil_check_test_2 : true = {} as Equal<'false', Cion.Lisp<`(nil? true)`>>
const nil_check_test_3 : true = {} as Equal<'false', Cion.Lisp<`(nil? false)`>>
const nil_check_test_4 : true = {} as Equal<'true',  Cion.Lisp<`(nil? (let [n nil] n))`>>

// Threading combined with logical ops
const thread_comb_test_0 : true = {} as Equal<'true', Cion.Lisp<`(some-> 1 number? (-> nil? not))`>>
const thread_comb_test_1 : true = {} as Equal<'true', Cion.Lisp<`(if (-> (if (number? 1) (number? 1) false) nil? not) (-> (if (number? 1) (number? 1) false) nil? not) false)`>>

// Functional transformation within threading
const some_arrow_fn_test_0 : true = {} as Equal<'2', Cion.Lisp<`(some-> 1 boolean? ((fn [n] (if (= n false) 2 4))))`>>
const some_arrow_fn_test_1 : true = {} as Equal<'2', Cion.Lisp<`(if ((fn [n] (if (= n false) 2 4)) (if (boolean? 1) (boolean? 1) false)) ((fn [n] (if (= n false) 2 4)) (if (boolean? 1) (boolean? 1) false)) nil)`>>

// Nested/Complex threading
const complex_thread_test_0 : true = {} as Equal<'true', Cion.Lisp<`(some-> 1 boolean? string? ((fn [n] (if (= n false) 2 -2))) pos-int?)`>>
const complex_thread_test_1 : true = {} as Equal<'true', Cion.Lisp<`((fn [n] (some-> n number?)) 1)`>>
const complex_thread_test_2 : true = {} as Equal<'2',    Cion.Lisp<`((fn [n] (some-> n inc)) 1)`>>
