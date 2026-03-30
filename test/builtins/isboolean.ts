import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const isboolean_test_0 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? 1)`>>
const isboolean_test_1 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? -1)`>>
const isboolean_test_2 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? 29438)`>>
const isboolean_test_3 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? '')`>>
const isboolean_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? '102')`>>
const isboolean_test_5 : true = {} as Equal<['prim', true], Cion.RawLisp<`(boolean? false)`>>
const isboolean_test_6 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? [])`>>
const isboolean_test_7 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? [1 2 3])`>>
const isboolean_test_8 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? (fn [x y] (+ x y)))`>>
const isboolean_test_9 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? {:x 1 :y 1})`>>
const isboolean_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? nil)`>>
const isboolean_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? (+ 1 2))`>>
const isboolean_test_12 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? ((fn [] (+ 1 2))))`>>
const isboolean_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? ((fn [x y] (+ x y)) 1 2))`>>
const isboolean_test_14 : true = {} as Equal<['prim', true], Cion.RawLisp<`(boolean? (:x {:x false}))`>>
const isboolean_test_15 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? (:x {:x 1}))`>>
const isboolean_test_16 : true = {} as Equal<['prim', true], Cion.RawLisp<`(boolean? (first [true '1' 2]))`>>
const isboolean_test_17 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? (first [0 '1' 2]))`>>
const isboolean_test_18 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? (let [x 1 y 2] (+ x y)))`>>
const isboolean_test_19 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? (let [] (+ 1 2)))`>>
const isboolean_test_20 : true = {} as Equal<['prim', false], Cion.RawLisp<`(boolean? (+ 3 (let [x 1 y 2] (+ x y))))`>>
