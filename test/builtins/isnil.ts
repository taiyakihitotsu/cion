import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const isnil_test_0 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? 1)`>>
const isnil_test_1 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? -1)`>>
const isnil_test_2 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? 29438)`>>
const isnil_test_3 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? '')`>>
const isnil_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? '102')`>>
const isnil_test_5 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? false)`>>
const isnil_test_6 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? [])`>>
const isnil_test_7 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? [1 2 3])`>>
const isnil_test_8 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? (fn [x y] (+ x y)))`>>
const isnil_test_9 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? {:x 1 :y 1})`>>
const isnil_test_10 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(nil? nil)`>>
const isnil_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? (+ 1 2))`>>
const isnil_test_12 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? ((fn [] (+ 1 2))))`>>
const isnil_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? ((fn [x y] (+ x y)) 1 2))`>>
const isnil_test_14 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? (:x {:x 1}))`>>
const isnil_test_15 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? (first [0 '1' 2]))`>>
const isnil_test_16 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? (let [x 1 y 2] (+ x y)))`>>
const isnil_test_17 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? (let [] (+ 1 2)))`>>
const isnil_test_18 : true = {} as Equal<['prim', false], Cion.RawLisp<`(nil? (+ 3 (let [x 1 y 2] (+ x y))))`>>
