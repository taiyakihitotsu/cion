import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const odd_test_0  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(odd? 1)`>>
const odd_test_1  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(odd? -1)`>>
const odd_test_2  : true = {} as Equal<['prim', false], Cion.RawLisp<`(odd? 2)`>>
const odd_test_3  : true = {} as Equal<['prim', false], Cion.RawLisp<`(odd? -2)`>>
const odd_test_4  : true = {} as Equal<['prim', false], Cion.RawLisp<`(odd? 29438)`>>
const odd_test_5  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(odd? 29439)`>>
const odd_test_6  : true = {} as Equal<['prim', false], Cion.RawLisp<`(odd? '')`>>
const odd_test_7  : true = {} as Equal<['prim', false], Cion.RawLisp<`(odd? '102')`>>
const odd_test_8  : true = {} as Equal<['prim', false], Cion.RawLisp<`(odd? true)`>>
const odd_test_9  : true = {} as Equal<['prim', false], Cion.RawLisp<`(odd? [])`>>
const odd_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(odd? [1 2 3])`>>
const odd_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(odd? (fn [x y] (+ x y)))`>>
const odd_test_12 : true = {} as Equal<['prim', false], Cion.RawLisp<`(odd? {:x 1 :y 1})`>>
const odd_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(odd? nil)`>>
const odd_test_14 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(odd? (+ 1 2))`>>
const odd_test_15 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(odd? ((fn [] (+ 1 2))))`>>
const odd_test_16 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(odd? ((fn [x y] (+ x y)) 1 2))`>>
const odd_test_17 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(odd? (:x {:x 1}))`>>
const odd_test_18 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(odd? (first [-1 '1' 2]))`>>
const odd_test_19 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(odd? (let [x 1 y 2] (+ x y)))`>>
const odd_test_20 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(odd? (let [] (+ 1 2)))`>>
const odd_test_21 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(odd? (+ 4 (let [x 1 y 2] (+ x y))))`>>
