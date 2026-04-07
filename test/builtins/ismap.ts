import type Cion from '../../src/index.js'
import type {Equal} from '../../src/util.js'

const ismap_test_0 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? 1)`>>
const ismap_test_1 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? -1)`>>
const ismap_test_2 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? 29438)`>>
const ismap_test_3 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? '')`>>
const isismap_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? '102')`>>
const ismap_test_5 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? false)`>>
const ismap_test_6 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? [])`>>
const ismap_test_7 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? [1 2 3])`>>
const ismap_test_8 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (fn [x y] (+ x y)))`>>
const ismap_test_9 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(map? {:x 1 :y 1})`>>
const ismap_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? nil)`>>
const ismap_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (+ 1 2))`>>
const ismap_test_12 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? ((fn [] (+ 1 2))))`>>
const ismap_test_13 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? ((fn [x y] (+ x y)) 1 2))`>>
const ismap_test_14 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (:x {:x 1}))`>>
const ismap_test_15 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (first [0 '1' 2]))`>>
const ismap_test_16 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (let [x 1 y 2] (+ x y)))`>>
const ismap_test_17 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (let [] (+ 1 2)))`>>
const ismap_test_18 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (+ 3 (let [x 1 y 2] (+ x y))))`>>
const ismap_test_19 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? ((fn [] (str 'a' 'b'))))`>>
const ismap_test_20 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? ((fn [x y] (str x y)) 'a' 'b'))`>>
const ismap_test_21 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (:x {:x '111'}))`>>
const ismap_test_22 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (first ['0' '1' 2]))`>>
const ismap_test_23 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (let [x 'a' y 'b'] (str x y)))`>>
const ismap_test_24 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (let [] (str 'a' 'b')))`>>
const ismap_test_25 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (str 'c' (let [x 'a' y 'b'] (str x y))))`>>
const ismap_test_26 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(map? ((fn [] {:x 0 :y 1})))`>>
const ismap_test_27 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(map? ((fn [x y] {:x x :y y}) 'a' 'b'))`>>
const ismap_test_28 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(map? (:x {:x {:y 1}}))`>>
const ismap_test_29 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(map? (first [{:x '0'} '1' 2]))`>>
const ismap_test_30 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(map? (let [x 'a' y 'b'] {:x x :y y}))`>>
const ismap_test_31 : true = {} as Equal<['prim', true],  Cion.RawLisp<`(map? (let [] {:x 'a' :y 'b'}))`>>
const ismap_test_32 : true = {} as Equal<['prim', false], Cion.RawLisp<`(map? (assoc (let [x 'a' y 'b'] {:x x :y y}) :c 2))`>>
