import type Cion from '../src/index'

const abs_test0 : Cion.Lisp<`(abs 0)`> = '0'
const abs_test1 : Cion.Lisp<`(abs 1)`> = '1'
const abs_test2 : Cion.Lisp<`(abs -1)`> = '1'
const abs_test3 : Cion.Lisp<`(abs 2/3)`> = '2/3'
const abs_test4 : Cion.Lisp<`(abs -2/3)`> = '2/3'
const abs_test5n : Cion.Lisp<`(abs nil)`> = 'nil'
const abs_test5 : Cion.Lisp<`(abs (* -2 -3/5))`> = '6/5'

