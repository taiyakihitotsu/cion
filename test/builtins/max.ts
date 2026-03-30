import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const max_test_0 : true = {} as Equal<'3',  Cion.Lisp<`(max 1 3)`>>
const max_test_1 : true = {} as Equal<'3',  Cion.Lisp<`(max 1 3 -2)`>>
const max_test_2 : true = {} as Equal<'1',  Cion.Lisp<`(max 1 -2/9 -2)`>>
const max_test_3 : true = {} as Equal<'1',  Cion.Lisp<`(max 0 1 -9/2 -2)`>>
const max_test_4 : true = {} as Equal<'87', Cion.Lisp<`(max 9 87 0 1)`>>
const max_test_5 : true = {} as Equal<'0',  Cion.Lisp<`(max -9 -87 0 -1)`>>
