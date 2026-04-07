import type Cion from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const range_test_0 : true = {} as Equal<'[0]',               Cion.Lisp<`(range 0 1)`>>
const range_test_1 : true = {} as Equal<'[1 2]',             Cion.Lisp<`(range 1 3)`>>
const range_test_2 : true = {} as Equal<'[]',                Cion.Lisp<`(range 3 3)`>>
const range_test_3 : true = {} as Equal<'[]',                Cion.Lisp<`(range 5 1)`>>
const range_test_4 : true = {} as Equal<'[-5 -4 -3 -2 -1 0]', Cion.Lisp<`(range -5 1)`>>
const range_test_5 : true = {} as Equal<'[]',                Cion.Lisp<`(range 5 -1)`>>
