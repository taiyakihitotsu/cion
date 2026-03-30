import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const pos_test_0  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos? 1)`>>
const pos_test_1  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? -1)`>>
const pos_test_2  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos? 29438)`>>
const pos_test_3  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? '')`>>
const pos_test_4  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? '102')`>>
const pos_test_5  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? true)`>>
const pos_test_6  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? [])`>>
const pos_test_7  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? [1 2 3])`>>
const pos_test_8  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? (fn [x y] (+ x y)))`>>
const pos_test_9  : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? {:x 1 :y 1})`>>
const pos_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? nil)`>>
const pos_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? (- 1 2))`>>
const pos_test_12 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? ((fn [] (- 1 2))))`>>
const pos_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? ((fn [x y] (- x y)) 1 2))`>>
const pos_test_14 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? (:x {:x -1}))`>>
const pos_test_15 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? (first [-1 '1' 2]))`>>
const pos_test_16 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? (let [x 1 y 2] (- x y)))`>>
const pos_test_17 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? (let [] (- 1 2)))`>>
const pos_test_18 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? (* 3 (let [x 1 y 2] (- x y))))`>>

const pos_test_19 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos? 1/2)`>>
const pos_test_20 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? -1/2)`>>
const pos_test_21 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? -3/1)`>>
const pos_test_22 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos? 3/1)`>>
const pos_test_23 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(pos? -3/-1)`>>
const pos_test_24 : true = {} as Equal<['prim', false], Cion.RawLisp<`(pos? 3/-1)`>>
