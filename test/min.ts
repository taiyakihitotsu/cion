import type Cion from '../src/index'

const min_test_0 : Cion.Lisp<`(min 1 3)`> = '1'
const min_test_1 : Cion.Lisp<`(min 1 3 -2)`> = '-2'
const min_test_2 : Cion.Lisp<`(min 1 -2/9 -2)`> = '-2'
const min_test_3 : Cion.Lisp<`(min 0 1 -9/2 -2)`> = '-9/2'
const min_test_4 : Cion.Lisp<`(min 9 87 0 1)`> = '0'

