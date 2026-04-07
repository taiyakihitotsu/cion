import type Cion from '../../src/index.js'
import type {Equal} from '../../src/util.js'

const isempty_test_0 : true = {} as Equal<['prim', false], Cion.RawLisp<`(empty? 1)`>>
const isempty_test_1 : true = {} as Equal<['prim', false], Cion.RawLisp<`(empty? -1)`>>
const isempty_test_2 : true = {} as Equal<['prim', false], Cion.RawLisp<`(empty? 29438)`>>
const isempty_test_3 : true = {} as Equal<['prim', false], Cion.RawLisp<`(empty? '')`>>
const isempty_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<`(empty? '102')`>>
const isempty_test_5 : true = {} as Equal<['prim', false], Cion.RawLisp<`(empty? true)`>>
const isempty_test_6 : true = {} as Equal<['prim', true], Cion.RawLisp<`(empty? [])`>>
const isempty_test_7 : true = {} as Equal<['prim', true], Cion.RawLisp<`(empty? {})`>>
const isempty_test_8 : true = {} as Equal<['prim', false], Cion.RawLisp<`(empty? [1 2 3])`>>
const isempty_test_9 : true = {} as Equal<['prim', false], Cion.RawLisp<`(empty? (fn [x y] (+ x y)))`>>
const isempty_test_10 : true = {} as Equal<['prim', false], Cion.RawLisp<`(empty? {:x 1 :y 1})`>>
const isempty_test_11 : true = {} as Equal<['prim', false], Cion.RawLisp<`(empty? nil)`>>
const isempty_test_12 : true = {} as Equal<['prim', true], Cion.RawLisp<`(empty? (rest [1]))`>>
const isempty_test_13 : true = {} as Equal<['prim', true], Cion.RawLisp<`(empty? ((fn [] [])))`>>
const isempty_test_14 : true = {} as Equal<['prim', true], Cion.RawLisp<`(empty? ((fn [x] (rest x)) [1]))`>>
const isempty_test_15 : true = {} as Equal<['prim', true], Cion.RawLisp<`(empty? (:x {:x []}))`>>
const isempty_test_16 : true = {} as Equal<['prim', true], Cion.RawLisp<`(empty? (first [[] '1' 2]))`>>
const isempty_test_17 : true = {} as Equal<['prim', true], Cion.RawLisp<`(empty? (let [x 1 y 2] (rest (rest [x y]))))`>>
const isempty_test_18 : true = {} as Equal<['prim', true], Cion.RawLisp<`(empty? (let [] []))`>>
