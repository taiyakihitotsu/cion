import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const isifn_test_0 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? 1)`>>
const isifn_test_1 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? -1)`>>
const isifn_test_2 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? 29438)`>>
const isifn_test_3 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? '')`>>
const isifn_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? '102')`>>
const isifn_test_5 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? false)`>>
const isifn_test_6 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? [])`>>
const isifn_test_7 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? [1 2 3])`>>
const isifn_test_8 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(ifn? (fn [x y] (+ x y)))`>>
const isifn_test_9 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(ifn? inc)`>>
const isifn_test_10 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(ifn? {:x 1 :y 1})`>>
const isifn_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? nil)`>>
const isifn_test_12 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(ifn? :key)`>>
const isifn_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? (+ 1 2))`>>
const isifn_test_14 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? ((fn [] (+ 1 2))))`>>
const isifn_test_15 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? ((fn [x y] (+ x y)) 1 2))`>>
const isifn_test_16 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? (:x {:x 1}))`>>
const isifn_test_17 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? (first [0 '1' 2]))`>>
const isifn_test_18 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? (let [x 1 y 2] (+ x y)))`>>
const isifn_test_19 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? (let [] (+ 1 2)))`>>
const isifn_test_20 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? (+ 3 (let [x 1 y 2] (+ x y))))`>>
const isifn_test_21 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? ((fn [] (str 'a' 'b'))))`>>
const isifn_test_22 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? ((fn [x y] (str x y)) 'a' 'b'))`>>
const isifn_test_23 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? (:x {:x '111'}))`>>
const isifn_test_24 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? (first ['0' '1' 2]))`>>
const isifn_test_25 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? (let [x 'a' y 'b'] (str x y)))`>>
const isifn_test_26 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? (let [] (str 'a' 'b')))`>>
const isifn_test_27 : true = {} as Equal<['prim', false], Cion.RawLisp<`(ifn? (str 'c' (let [x 'a' y 'b'] (str x y))))`>>
const isifn_test_28 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(ifn? ((fn [] ((fn [x y] :key)))))`>>
const isifn_test_29 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(ifn? (:x {:x :key}))`>>
const isifn_test_30 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(ifn? (first [:key '1' 2]))`>>
const isifn_test_31 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(ifn? (let [x :a y :b] ((fn [] x))))`>>
const isifn_test_32 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(ifn? ((fn [a] ((let [x :a y :b] (fn [z] :key)) a)) 1))`>>
