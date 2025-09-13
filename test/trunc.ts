import type Cion from '../src/index'

const trunc_test_0: Cion.Lisp<`(trunc 0)`> = '0'
const trunc_test_1: Cion.Lisp<`(trunc 2)`> = '2'
const trunc_test_2: Cion.Lisp<`(trunc -2)`> = '-2'
const trunc_test_3: Cion.Lisp<`(trunc 3/2)`> = '1'
const trunc_test_4: Cion.Lisp<`(trunc -3/2)`> = '-1'
const trunc_test_5: Cion.Lisp<`(trunc 7/3)`> = '2'
const trunc_test_6: Cion.Lisp<`(trunc -7/3)`> = '-2'
const trunc_test_7: Cion.Lisp<`(trunc 1/2)`> = '0'
const trunc_test_8: Cion.Lisp<`(trunc -1/2)`> = '0'

