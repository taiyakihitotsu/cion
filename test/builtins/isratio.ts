import type Cion from '../../src/index.js'
import type {Equal} from '../../src/util.js'

const ratio_test_0  : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? 1)`>>
const ratio_test_1  : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? -1)`>>
const ratio_test_2  : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? 29438)`>>
const ratio_test_3  : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? '')`>>
const ratio_test_4  : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? '102')`>>
const ratio_test_5  : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? false)`>>
const ratio_test_6  : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? [])`>>
const ratio_test_7  : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? [1 2 3])`>>
const ratio_test_8  : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? (fn [x y] (+ x y)))`>>
const ratio_test_9  : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? {:x 1 :y 1})`>>
const ratio_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? nil)`>>
const ratio_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? (+ 1 2))`>>
const ratio_test_12 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? ((fn [] (+ 1 2))))`>>
const ratio_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? ((fn [x y] (+ x y)) 1 2))`>>
const ratio_test_14 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? inc)`>>
const ratio_test_15 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? (:x {:x 1}))`>>
const ratio_test_16 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? (first [0 '1' 2]))`>>
const ratio_test_17 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? (let [x 1 y 2] (+ x y)))`>>
const ratio_test_18 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? (let [] (+ 1 2)))`>>
const ratio_test_19 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? (+ 3 (let [x 1 y 2] (+ x y))))`>>
const ratio_test_20 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? ((fn [] (str 'a' 'b'))))`>>

const ratio_test_21 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(ratio? 1/2)`>>
const ratio_test_22 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(ratio? -1/2)`>>
const ratio_test_23 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? -3/1)`>>
const ratio_test_24 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? 3/1)`>>
const ratio_test_25 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? -3/-1)`>>
const ratio_test_26 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ratio? 3/-1)`>>
