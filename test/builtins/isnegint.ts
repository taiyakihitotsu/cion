import type Cion from '../../src/index.js'
import type {Equal} from '../../src/util.js'

const isnegint_test_0 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? 1)`>>
const isnegint_test_1 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg-int? -1)`>>
const isnegint_test_2 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? 29438)`>>
const isnegint_test_3 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? '')`>>
const isnegint_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? '102')`>>
const isnegint_test_5 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? true)`>>
const isnegint_test_6 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? [])`>>
const isnegint_test_7 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? [1 2 3])`>>
const isnegint_test_8 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? (fn [x y] (+ x y)))`>>
const isnegint_test_9 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? {:x 1 :y 1})`>>
const isnegint_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? nil)`>>
const isnegint_test_11 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg-int? (- 1 2))`>>
const isnegint_test_12 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg-int? ((fn [] (- 1 2))))`>>
const isnegint_test_13 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg-int? ((fn [x y] (- x y)) 1 2))`>>
const isnegint_test_14 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg-int? (:x {:x -1}))`>>
const isnegint_test_15 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg-int? (first [-1 '1' 2]))`>>
const isnegint_test_16 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg-int? (let [x 1 y 2] (- x y)))`>>
const isnegint_test_17 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg-int? (let [] (- 1 2)))`>>
const isnegint_test_18 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg-int? (* 3 (let [x 1 y 2] (- x y))))`>>

// Rational
const isnegint_test_19 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? 1/2)`>>
const isnegint_test_20 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? -1/2)`>>
const isnegint_test_21 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg-int? -3/1)`>>
const isnegint_test_22 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? 3/1)`>>
const isnegint_test_23 : true = {} as Equal<['prim', false], Cion.RawLisp<`(neg-int? -3/-1)`>>
const isnegint_test_24 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(neg-int? 3/-1)`>>
