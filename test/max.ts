import type Cion from '../src/index'

const max_test_0 : Cion.Lisp<`(max 1 3)`> = '3'
const max_test_1 : Cion.Lisp<`(max 1 3 -2)`> = '3'
const max_test_2 : Cion.Lisp<`(max 1 -2/9 -2)`> = '1'
const max_test_3 : Cion.Lisp<`(max 0 1 -9/2 -2)`> = '1'
const max_test_4 : Cion.Lisp<`(max 9 87 0 1)`> = '87'
const max_test_5 : Cion.Lisp<`(max -9 -87 0 -1)`> = '0'
