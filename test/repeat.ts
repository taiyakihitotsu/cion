import type Cion from '../src/index'

const repeat_test0 : Cion.Lisp<`(repeat 0 'x')`> = '[]'
const repeat_test1 : Cion.Lisp<`(repeat 2 'x')`> = `['x' 'x']`
const repeat_test3 : Cion.Lisp<`(repeat -1 'x')`> = '[]'
const repeat_test4 : Cion.Lisp<`(repeat 3 (fn [n] (inc n)))`> = '[(fn [n] (inc n)) (fn [n] (inc n)) (fn [n] (inc n))]'
