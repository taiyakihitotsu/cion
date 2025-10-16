import type Cion from '../src/index'
import type { LispIsEvery } from '../src/index'

const isevery_test0: LispIsEvery<[['fn', [['sym', 'x']], [['sym', 'number?'], ['sym', 'x']]], ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001']]]> = ['prim', true]
const isevery_test1: LispIsEvery<[['fn', [['sym', 'x']], [['sym', 'number?'], ['sym', 'x']]], ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', `'0000000000000001'`]]]> = ['prim', false]
const isevery_test2: LispIsEvery<[['fn', [['sym', 'x']], [['sym', 'number?'], ['sym', 'x']]], ['vec']]> = ['prim', false]

const isevery_list_test0: Cion.Lisp<`(every? (fn [x] (number? x)) [0 1 2])`> = 'true'
const isevery_list_test1: Cion.Lisp<`(every? number? [0 1 2])`> = 'true'
const isevery_list_test2: Cion.Lisp<`(every? number? [0 1 '2'])`> = 'false'
