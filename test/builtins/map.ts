import type Cion from '../../src/index.js'
import type {Equal} from '../../src/util.js'
import { VNil } from '../../src/sexprtypes.js'

const map_lookup_test_0 : true = {} as Equal<['prim', '0000000000000001'], Cion.RawLisp<"(:a {:a 1})">>
const map_lookup_test_1 : true = {} as Equal<['prim', '0000000000000001'], Cion.RawLisp<"(:a {:a 1 :b 2})">>
const map_lookup_test_2 : true = {} as Equal<['prim', '0000000000000001'], Cion.RawLisp<"({:a 1 :b 2} :a)">>
const map_lookup_test_3 : true = {} as Equal<typeof VNil, Cion.RawLisp<"(:c {:a 1 :b 2})">>
const map_lookup_test_4 : true = {} as Equal<typeof VNil, Cion.RawLisp<"({:a 1 :b 2} :c)">>
const map_lookup_test_5 : true = {} as Equal<['prim', '0000000000000001'], Cion.RawLisp<"(get {:a 1 :b 2} :a)">>

const map_literal_test_0 : true = {} as Equal<['map', [['key', ':a'], ['prim', '0000000000000001']]], Cion.RawLisp<"{:a 1}">>

const map_fn_test_0 : true = {} as Equal<'[1 2 3]', Cion.Lisp<`(map inc [0 1 2])`>>
const map_fn_test_1 : true = {} as Equal<'[]',      Cion.Lisp<`(map inc [])`>>
const map_fn_test_2 : true = {} as Equal<'[1 2 3]', Cion.Lisp<`(map (fn [n] (inc n)) [0 1 2])`>>
const map_fn_test_3 : true = {} as Equal<'[1 2 3]', Cion.Lisp<`(let [r 1] (map (fn [n] (+ r n)) [0 1 2]))`>>
const map_fn_test_4 : true = {} as Equal<'[1 2 3]', Cion.Lisp<`(let [r 1 f (fn [n] (+ r n))] (map f [0 1 2]))`>>
const map_fn_test_5 : true = {} as Equal<'[1 2 3]', Cion.Lisp<`(map (let [r 1] (fn [n] (+ r n))) [0 1 2])`>>
const map_fn_test_6 : true = {} as Equal<'[1 2 3]', Cion.Lisp<`(map (let [r 1 f (fn [n] (+ r n))] f) [0 1 2])`>>
const map_fn_test_7 : true = {} as Equal<'[1 2 3]', Cion.Lisp<`(map (let [f (fn [n] (+ 1 n))] f) [0 1 2])`>>
const map_fn_test_8 : true = {} as Equal<'[1 2 3]', Cion.Lisp<`(let [v [0 1 2]] (map (let [f (fn [n] (+ 1 n))] f) v))`>>
const map_fn_test_9 : true = {} as Equal<'[1 2 3]', Cion.Lisp<`(let [r 1 v [0 1 2]] (map (let [f (fn [n] (+ r n))] f) v))`>>
const map_fn_test_10 : true = {} as Equal<'[1 2 3]', Cion.Lisp<`((fn [x] (let [v x] (map (let [f (fn [n] (+ 1 n))] f) v))) [0 1 2])`>>
const map_fn_test_11 : true = {} as Equal<'[1 2 3]', Cion.Lisp<`((fn [i x] (let [v x] (map (let [f (fn [n] (+ i n))] f) v))) 1 [0 1 2])`>>

// @ts-expect-error:
const   maptest5 : Cion.Lisp<`(let [f (fn [n] (+ r n)) r 1] (map f [0 1 2]))`> = '[1 2 3]'
