import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const iskeyword_test_0 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? 1)`>>
const iskeyword_test_1 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? -1)`>>
const iskeyword_test_2 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? 29438)`>>
const iskeyword_test_3 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? '')`>>
const iskeyword_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? '102')`>>
const iskeyword_test_5 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? false)`>>
const iskeyword_test_6 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? [])`>>
const iskeyword_test_7 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? [1 2 3])`>>
const iskeyword_test_8 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (fn [x y] (+ x y)))`>>
const iskeyword_test_9 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? {:x 1 :y 1})`>>
const iskeyword_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? nil)`>>
const iskeyword_test_11 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(keyword? :key)`>>
const iskeyword_test_12 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (+ 1 2))`>>
const iskeyword_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? ((fn [] (+ 1 2))))`>>
const iskeyword_test_14 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? ((fn [x y] (+ x y)) 1 2))`>>
const iskeyword_test_15 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (:x {:x 1}))`>>
const iskeyword_test_16 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (first [0 '1' 2]))`>>
const iskeyword_test_17 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (let [x 1 y 2] (+ x y)))`>>
const iskeyword_test_18 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (let [] (+ 1 2)))`>>
const iskeyword_test_19 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (+ 3 (let [x 1 y 2] (+ x y))))`>>
const iskeyword_test_20 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? ((fn [] (str 'a' 'b'))))`>>
const iskeyword_test_21 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? ((fn [x y] (str x y)) 'a' 'b'))`>>
const iskeyword_test_22 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (:x {:x '111'}))`>>
const iskeyword_test_23 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (first ['0' '1' 2]))`>>
const iskeyword_test_24 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (let [x 'a' y 'b'] (str x y)))`>>
const iskeyword_test_25 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (let [] (str 'a' 'b')))`>>
const iskeyword_test_26 : true = {} as Equal<['prim', false], Cion.RawLisp<`(keyword? (str 'c' (let [x 'a' y 'b'] (str x y))))`>>
const iskeyword_test_27 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(keyword? ((fn [] ((fn [x y] :key)))))`>>
const iskeyword_test_28 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(keyword? (:x {:x :key}))`>>
const iskeyword_test_29 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(keyword? (first [:key '1' 2]))`>>
const iskeyword_test_30 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(keyword? (let [x :a y :b] ((fn [] x))))`>>
const iskeyword_test_31 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(keyword? ((fn [a] ((let [x :a y :b] (fn [z] :key)) a)) 1))`>>
