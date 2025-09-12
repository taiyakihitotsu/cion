import type Cion from '../src/index.ts'
 
const maintest0_remove_0: Cion.RawLisp<'(remove (fn [n] (> 3 n)) [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']]
const maintest0_remove_1: Cion.RawLisp<'(let [f (fn [n] (> 3 n))] (remove f [0 1 2 3 4 5]))'> = ['vec', ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']]

const test_remove_0: Cion.Lisp<`(remove number? [])`> = '[]'
const test_remove_1: Cion.Lisp<`(remove number? ['s' 0 1 false 2])`> = `['s' false]`
const test_remove_2: Cion.Lisp<`(remove number? [0 1 2])`> = '[]'
const test_remove_3: Cion.Lisp<`(remove number? [true false 's'])`> = `[true false 's']`
