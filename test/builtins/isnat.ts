import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const isnat_test_0 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(nat? 1)`>>
const isnat_test_1 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(nat? 0)`>>
const isnat_test_2 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? -1)`>>
const isnat_test_3 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(nat? 29438)`>>
const isnat_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? '')`>>
const isnat_test_5 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? '102')`>>
const isnat_test_6 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? true)`>>
const isnat_test_7 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? [])`>>
const isnat_test_8 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? [1 2 3])`>>
const isnat_test_9 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? (fn [x y] (+ x y)))`>>
const isnat_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? {:x 1 :y 1})`>>
const isnat_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? nil)`>>
const isnat_test_12 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? (- 1 2))`>>
const isnat_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? ((fn [] (- 1 2))))`>>
const isnat_test_14 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? ((fn [x y] (- x y)) 1 2))`>>
const isnat_test_15 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? (:x {:x -1}))`>>
const isnat_test_16 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? (first [-1 '1' 2]))`>>
const isnat_test_17 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? (let [x 1 y 2] (- x y)))`>>
const isnat_test_18 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? (let [] (- 1 2)))`>>
const isnat_test_19 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? (* 3 (let [x 1 y 2] (- x y))))`>>

// Rational
const isnat_test_20 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? 1/2)`>>
const isnat_test_21 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? -1/2)`>>
const isnat_test_22 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? -3/1)`>>
const isnat_test_23 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(nat? 3/1)`>>
const isnat_test_24 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(nat? -3/-1)`>>
const isnat_test_25 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nat? 3/-1)`>>
