import type Cion from '../../src/index.js'
import type {Equal} from '../../src/util.js'

const isint_test_0 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? 1)`>>
const isint_test_1 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? 0)`>>
const isint_test_2 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? -1)`>>
const isint_test_3 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? 29438)`>>
const isint_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(int? '')`>>
const isint_test_5 : true = {} as Equal<['prim', false], Cion.RawLisp<`(int? '102')`>>
const isint_test_6 : true = {} as Equal<['prim', false], Cion.RawLisp<`(int? true)`>>
const isint_test_7 : true = {} as Equal<['prim', false], Cion.RawLisp<`(int? [])`>>
const isint_test_8 : true = {} as Equal<['prim', false], Cion.RawLisp<`(int? [1 2 3])`>>
const isint_test_9 : true = {} as Equal<['prim', false], Cion.RawLisp<`(int? (fn [x y] (+ x y)))`>>
const isint_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(int? {:x 1 :y 1})`>>
const isint_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(int? nil)`>>
const isint_test_12 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? (- 1 2))`>>
const isint_test_13 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? ((fn [] (- 1 2))))`>>
const isint_test_14 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? ((fn [x y] (- x y)) 1 2))`>>
const isint_test_15 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? (:x {:x -1}))`>>
const isint_test_16 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? (first [-1 '1' 2]))`>>
const isint_test_17 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? (let [x 1 y 2] (- x y)))`>>
const isint_test_18 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? (let [] (- 1 2)))`>>
const isint_test_19 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? (* 3 (let [x 1 y 2] (- x y))))`>>

// Rational
const isint_test_20 : true = {} as Equal<['prim', false], Cion.RawLisp<`(int? 1/2)`>>
const isint_test_21 : true = {} as Equal<['prim', false], Cion.RawLisp<`(int? -1/2)`>>
const isint_test_22 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? -3/1)`>>
const isint_test_23 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? 3/1)`>>
const isint_test_24 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? -3/-1)`>>
const isint_test_25 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(int? 3/-1)`>>
