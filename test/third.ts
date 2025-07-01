import type Cion from '../src/index'


const third_test0: Cion.Lisp<`(third [0 1 2 3 4 5])`> = '2'
const third_test1: Cion.Lisp<`(third [0])`> = 'nil'
const third_test:  Cion.Lisp<`(third {:a 1})`> = 'nil'
