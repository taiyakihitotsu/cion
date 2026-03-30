import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const vector_test_0  : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? 1)`>>
const vector_test_1  : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? -1)`>>
const vector_test_2  : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? 29438)`>>
const vector_test_3  : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? '')`>>
const vector_test_4  : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? '102')`>>
const vector_test_5  : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? false)`>>
const vector_test_6  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(vector? [])`>>
const vector_test_7  : true = {} as Equal<['prim', true],  Cion.RawLisp<`(vector? [1 2 3])`>>
const vector_test_8  : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (fn [x y] (+ x y)))`>>
const vector_test_9  : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? {:x 1 :y 1})`>>
const vector_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? nil)`>>
const vector_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (+ 1 2))`>>
const vector_test_12 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? ((fn [] (+ 1 2))))`>>
const vector_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? ((fn [x y] (+ x y)) 1 2))`>>
const vector_test_14 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (:x {:x 1}))`>>
const vector_test_15 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (first [0 '1' 2]))`>>
const vector_test_16 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (let [x 1 y 2] (+ x y)))`>>
const vector_test_17 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (let [] (+ 1 2)))`>>
const vector_test_18 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (+ 3 (let [x 1 y 2] (+ x y))))`>>
const vector_test_19 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? ((fn [] (str 'a' 'b'))))`>>
const vector_test_20 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? ((fn [x y] (str x y)) 'a' 'b'))`>>
const vector_test_21 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (:x {:x '111'}))`>>
const vector_test_22 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (first ['0' '1' 2]))`>>
const vector_test_23 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (let [x 'a' y 'b'] (str x y)))`>>
const vector_test_24 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (let [] (str 'a' 'b')))`>>
const vector_test_25 : true = {} as Equal<['prim', false], Cion.RawLisp<`(vector? (str 'c' (let [x 'a' y 'b'] (str x y))))`>>

const vector_test_26 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(vector? ((fn [] [0 1])))`>>
const vector_test_27 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(vector? ((fn [x y] [x y]) 'a' 'b'))`>>
const vector_test_28 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(vector? (:x {:x ['111']}))`>>
const vector_test_29 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(vector? (first [['0'] '1' 2]))`>>
const vector_test_30 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(vector? (let [x 'a' y 'b'] [x y]))`>>
const vector_test_31 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(vector? (let [] ['a' 'b']))`>>
const vector_test_32 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(vector? ['c' (let [x 'a' y 'b'] [x y])])`>>
