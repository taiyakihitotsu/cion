import type { Cion } from '../../src/index'
import type { Equal } from '../../src/util'

const zero_lisp_test_0 : true = {} as Equal<'true',  Cion.Lisp<`(zero? 0)`>>
const zero_lisp_test_1 : true = {} as Equal<'false', Cion.Lisp<`(zero? 1)`>>
const zero_lisp_test_2 : true = {} as Equal<'false', Cion.Lisp<`(zero? [])`>>
const zero_lisp_test_3 : true = {} as Equal<'false', Cion.Lisp<`(zero? nil)`>>
const zero_lisp_test_4 : true = {} as Equal<'false', Cion.Lisp<`(-> 0 zero? not)`>>

const zero_raw_test_0  : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? 1)`>>
const zero_raw_test_1  : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? -1)`>>
const zero_raw_test_2  : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? 29438)`>>
const zero_raw_test_3  : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? '')`>>
const zero_raw_test_4  : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? '102')`>>
const zero_raw_test_5  : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? false)`>>
const zero_raw_test_6  : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? [])`>>
const zero_raw_test_7  : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? [1 2 3])`>>
const zero_raw_test_8  : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? (fn [x y] (+ x y)))`>>
const zero_raw_test_9  : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? {:x 1 :y 1})`>>
const zero_raw_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? nil)`>>
const zero_raw_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? (+ 1 2))`>>
const zero_raw_test_12 : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? ((fn [] (+ 1 2))))`>>
const zero_raw_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? ((fn [x y] (+ x y)) 1 2))`>>
const zero_raw_test_14 : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? (:x {:x 1}))`>>
const zero_raw_test_15 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(zero? (first [0 '1' 2]))`>>
const zero_raw_test_16 : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? (let [x 1 y 2] (+ x y)))`>>
const zero_raw_test_17 : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? (let [] (+ 1 2)))`>>
const zero_raw_test_18 : true = {} as Equal<['prim', false], Cion.RawLisp<`(zero? (+ 3 (let [x 1 y 2] (+ x y))))`>>
