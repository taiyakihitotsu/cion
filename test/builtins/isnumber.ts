import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const isnumber_test_0 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(number? 1)`>>
const isnumber_test_1 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(number? -1)`>>
const isnumber_test_2 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(number? 29438)`>>
const isnumber_test_3 : true = {} as Equal<['prim', false], Cion.RawLisp<`(number? '')`>>
const isnumber_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(number? '102')`>>
const isnumber_test_5 : true = {} as Equal<['prim', false], Cion.RawLisp<`(number? false)`>>
const isnumber_test_6 : true = {} as Equal<['prim', false], Cion.RawLisp<`(number? [])`>>
const isnumber_test_7 : true = {} as Equal<['prim', false], Cion.RawLisp<`(number? [1 2 3])`>>
const isnumber_test_8 : true = {} as Equal<['prim', false], Cion.RawLisp<`(number? (fn [x y] (+ x y)))`>>
const isnumber_test_9 : true = {} as Equal<['prim', false], Cion.RawLisp<`(number? {:x 1 :y 1})`>>
const isnumber_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(number? nil)`>>
const isnumber_test_11 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(number? (+ 1 2))`>>
const isnumber_test_12 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(number? ((fn [] (+ 1 2))))`>>
const isnumber_test_13 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(number? ((fn [x y] (+ x y)) 1 2))`>>
const isnumber_test_14 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(number? (:x {:x 1}))`>>
const isnumber_test_15 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(number? (first [0 '1' 2]))`>>
const isnumber_test_16 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(number? (let [x 1 y 2] (+ x y)))`>>
const isnumber_test_17 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(number? (let [] (+ 1 2)))`>>
const isnumber_test_18 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(number? (+ 3 (let [x 1 y 2] (+ x y))))`>>
