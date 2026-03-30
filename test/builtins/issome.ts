import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const some_test_0  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? 1)`>>
const some_test_1  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? -1)`>>
const some_test_2  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? 29438)`>>
const some_test_3  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? '')`>>
const some_test_4  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? '102')`>>
const some_test_5  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? true)`>>
const some_test_6  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? [])`>>
const some_test_7  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? [1 2 3])`>>
const some_test_8  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? (fn [x y] (+ x y)))`>>
const some_test_9  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? {:x 1 :y 1})`>>
const some_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(some? nil)`>>
const some_test_11 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? (+ 1 2))`>>
const some_test_12 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? ((fn [] (+ 1 2))))`>>
const some_test_13 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? ((fn [x y] (+ x y)) 1 2))`>>
const some_test_14 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? (:x {:x 1}))`>>
const some_test_15 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? (first [0 '1' 2]))`>>
const some_test_16 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? (let [x 1 y 2] (+ x y)))`>>
const some_test_17 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? (let [] (+ 1 2)))`>>
const some_test_18 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(some? (+ 3 (let [x 1 y 2] (+ x y))))`>>
