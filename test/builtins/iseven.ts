import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const iseven_test_0 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? 1)`>>
const iseven_test_1 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? -1)`>>
const iseven_test_2 : true = {} as Equal<['prim', true], Cion.RawLisp<`(even? 2)`>>
const iseven_test_3 : true = {} as Equal<['prim', true], Cion.RawLisp<`(even? -2)`>>
const iseven_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? 1/2)`>>
const iseven_test_5 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? -1/2)`>>
const iseven_test_6 : true = {} as Equal<['prim', true], Cion.RawLisp<`(even? 29438)`>>
const iseven_test_7 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? '')`>>
const iseven_test_8 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? '102')`>>
const iseven_test_9 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? true)`>>
const iseven_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? [])`>>
const iseven_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? [1 2 3])`>>
const iseven_test_12 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? (fn [x y] (+ x y)))`>>
const iseven_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? {:x 1 :y 1})`>>
const iseven_test_14 : true = {} as Equal<['prim', false], Cion.RawLisp<`(even? nil)`>>
const iseven_test_15 : true = {} as Equal<['prim', true], Cion.RawLisp<`(even? (+ 2 2))`>>
const iseven_test_16 : true = {} as Equal<['prim', true], Cion.RawLisp<`(even? ((fn [] (+ 2 2))))`>>
const iseven_test_17 : true = {} as Equal<['prim', true], Cion.RawLisp<`(even? ((fn [x y] (+ x y)) 2 2))`>>
const iseven_test_18 : true = {} as Equal<['prim', true], Cion.RawLisp<`(even? (:x {:x 2}))`>>
const iseven_test_19 : true = {} as Equal<['prim', true], Cion.RawLisp<`(even? (first [0 '1' 2]))`>>
const iseven_test_20 : true = {} as Equal<['prim', true], Cion.RawLisp<`(even? (let [x 2 y 2] (+ x y)))`>>
const iseven_test_21 : true = {} as Equal<['prim', true], Cion.RawLisp<`(even? (let [] (+ 2 2)))`>>
const iseven_test_22 : true = {} as Equal<['prim', true], Cion.RawLisp<`(even? (+ 3 (let [x 1 y 2] (+ x y))))`>>
