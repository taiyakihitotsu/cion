import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const isneg_test_0 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? 1)`>>
const isneg_test_1 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? -1)`>>
const isneg_test_2 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? 29438)`>>
const isneg_test_3 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? '')`>>
const isneg_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? '102')`>>
const isneg_test_5 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? true)`>>
const isneg_test_6 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? [])`>>
const isneg_test_7 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? [1 2 3])`>>
const isneg_test_8 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? (fn [x y] (+ x y)))`>>
const isneg_test_9 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? {:x 1 :y 1})`>>
const isneg_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? nil)`>>
const isneg_test_11 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? (- 1 2))`>>
const isneg_test_12 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? ((fn [] (- 1 2))))`>>
const isneg_test_13 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? ((fn [x y] (- x y)) 1 2))`>>
const isneg_test_14 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? (:x {:x -1}))`>>
const isneg_test_15 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? (first [-1 '1' 2]))`>>
const isneg_test_16 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? (let [x 1 y 2] (- x y)))`>>
const isneg_test_17 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? (let [] (- 1 2)))`>>
const isneg_test_18 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? (* 3 (let [x 1 y 2] (- x y))))`>>

// Rational
const isneg_test_19 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? 1/2)`>>
const isneg_test_20 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? -1/2)`>>
const isneg_test_21 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? -3/1)`>>
const isneg_test_22 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? 3/1)`>>
const isneg_test_23 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg? -3/-1)`>>
const isneg_test_24 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg? 3/-1)`>>
