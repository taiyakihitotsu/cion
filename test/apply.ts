import type Cion from '../src/index.ts'


const applytest_0: Cion.Lisp<`(apply inc [0])`> = '1'
const applytest_1: Cion.Lisp<`(apply + [0 1 2])`> = '3'
const applytest_2: Cion.Lisp<`(apply max [-1 0 1])`> = '1'

const applytest_0f: Cion.Lisp<`(apply (fn [n] (inc n)) [0])`> = '1'
const applytest_1f: Cion.Lisp<`(apply (fn [x y z] (+ x y z)) [0 1 2])`> = '3'
const applytest_2f: Cion.Lisp<`(apply (fn [x y z] (max x y z)) [-1 0 1])`> = '1'
