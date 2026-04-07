import type Cion from '../../src/index.js'
import type {Equal} from '../../src/util.js'

const pos_int_test_0  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? 1)`>>
const pos_int_test_1  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? -1)`>>
const pos_int_test_2  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? 29438)`>>
const pos_int_test_3  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? '')`>>
const pos_int_test_4  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? '102')`>>
const pos_int_test_5  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? true)`>>
const pos_int_test_6  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? [])`>>
const pos_int_test_7  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? [1 2 3])`>>
const pos_int_test_8  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? (fn [x y] (+ x y)))`>>
const pos_int_test_9  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? {:x 1 :y 1})`>>
const pos_int_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? nil)`>>
const pos_int_test_11 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? (+ 1 2))`>>
const pos_int_test_12 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? ((fn [] (+ 1 2))))`>>
const pos_int_test_13 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? ((fn [x y] (+ x y)) 1 2))`>>
const pos_int_test_14 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? (:x {:x 1}))`>>
const pos_int_test_15 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? (first [0 '1' 2]))`>>
const pos_int_test_16 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? (let [x 1 y 2] (+ x y)))`>>
const pos_int_test_17 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? (let [] (+ 1 2)))`>>
const pos_int_test_18 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? (+ 3 (let [x 1 y 2] (+ x y))))`>>

const pos_int_test_19 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? 1/2)`>>
const pos_int_test_20 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? -1/2)`>>
const pos_int_test_21 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? -3/1)`>>
const pos_int_test_22 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? 3/1)`>>
const pos_int_test_23 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos-int? -3/-1)`>>
const pos_int_test_24 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos-int? 3/-1)`>>
