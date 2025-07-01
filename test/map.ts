import type Cion from '../src/index.ts'
import { VNil } from '../src/sexprtypes'

const maintest12_get_0: Cion.RawLisp<"(:a {:a 1})"> = ['prim', '0000000000000001']
const maintest12_get_1: Cion.RawLisp<"(:a {:a 1 :b 2})"> = ['prim', '0000000000000001']
const maintest12_get_2: Cion.RawLisp<"({:a 1 :b 2} :a)"> = ['prim', '0000000000000001']
const maintest12_get_3: Cion.RawLisp<"(:c {:a 1 :b 2})"> = VNil
const maintest12_get_4: Cion.RawLisp<"({:a 1 :b 2} :c)"> = VNil
const maintest12_get_5: Cion.RawLisp<"(get {:a 1 :b 2} :a)"> = ['prim', '0000000000000001']

const maintest0_mapst_0: Cion.RawLisp<"{:a 1}"> = ['map', [['key', ':a'], ['prim', '0000000000000001']]]

const maptest : Cion.Lisp<`(map inc [0 1 2])`> = '[1 2 3]'
const maptest1 : Cion.Lisp<`(map inc [])`> = '[]'
const maptest2 : Cion.Lisp<`(map (fn [n] (inc n)) [0 1 2])`> = '[1 2 3]'
const maptest3 : Cion.Lisp<`(let [r 1] (map (fn [n] (+ r n)) [0 1 2]))`> = '[1 2 3]'
const  maptest4 : Cion.Lisp<`(let [r 1 f (fn [n] (+ r n))] (map f [0 1 2]))`> = '[1 2 3]'
// const  maptest4dump : Cion.Lisp<`(let [r 1 f (fn [n] (+ r n))] (map f [0 1 2]))`>['ast']['sexpr'] = '[1 2 3]'
// @ts-expect-error:
const   maptest5 : Cion.Lisp<`(let [f (fn [n] (+ r n)) r 1] (map f [0 1 2]))`> = '[1 2 3]'
const maptest6 : Cion.Lisp<`(map (let [r 1] (fn [n] (+ r n))) [0 1 2])`> = '[1 2 3]'
const  maptest7 : Cion.Lisp<`(map (let [r 1 f (fn [n] (+ r n))] f) [0 1 2])`> = '[1 2 3]'
const maptest8 : Cion.Lisp<`(map (let [f (fn [n] (+ 1 n))] f) [0 1 2])`> = '[1 2 3]'
const maptest9 : Cion.Lisp<`(let [v [0 1 2]] (map (let [f (fn [n] (+ 1 n))] f) v))`> = '[1 2 3]'
const  maptest10 : Cion.Lisp<`(let [r 1 v [0 1 2]] (map (let [f (fn [n] (+ r n))] f) v))`> = '[1 2 3]'
const  maptest11 : Cion.Lisp<`((fn [x] (let [v x] (map (let [f (fn [n] (+ 1 n))] f) v))) [0 1 2])`> = '[1 2 3]'
const  maptest12 : Cion.Lisp<`((fn [i x] (let [v x] (map (let [f (fn [n] (+ i n))] f) v))) 1 [0 1 2])`> = '[1 2 3]'
