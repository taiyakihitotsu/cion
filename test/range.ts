import type Cion from '../src/index'

const test_range0 : Cion.Lisp<`(range 0 1)`> = '[0]'
const test_range1 : Cion.Lisp<`(range 1 3)`> = '[1 2]'
const test_range2 : Cion.Lisp<`(range 3 3)`> = '[]'
const test_range3 : Cion.Lisp<`(range 5 1)`> = '[]'
const test_range4 : Cion.Lisp<`(range -5 1)`> = '[-5 -4 -3 -2 -1 0]'
const test_range5 : Cion.Lisp<`(range 5 -1)`> = '[]'
