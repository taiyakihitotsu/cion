import type Cion from '../src/index'

const floor_test_0: Cion.Lisp<`(floor 0)`> = '0'
const floor_test_1: Cion.Lisp<`(floor 2)`> = '2'
const floor_test_2: Cion.Lisp<`(floor -2)`> = '-2'
const floor_test_3: Cion.Lisp<`(floor 3/2)`> = '1'
const floor_test_4: Cion.Lisp<`(floor -3/2)`> = '-2'
const floor_test_5: Cion.Lisp<`(floor 7/3)`> = '2'
const floor_test_6: Cion.Lisp<`(floor -7/3)`> = '-3'
const floor_test_7: Cion.Lisp<`(floor 1/2)`> = '0'
const floor_test_8: Cion.Lisp<`(floor -1/2)`> = '-1'

