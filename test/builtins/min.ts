import type Cion from '../../src/index.ts'
import type {Equal} from '../../src/util'

const min_test_0 : true = {} as Equal<'1',    Cion.Lisp<`(min 1 3)`>>
const min_test_1 : true = {} as Equal<'-2',   Cion.Lisp<`(min 1 3 -2)`>>
const min_test_2 : true = {} as Equal<'-2',   Cion.Lisp<`(min 1 -2/9 -2)`>>
const min_test_3 : true = {} as Equal<'-9/2', Cion.Lisp<`(min 0 1 -9/2 -2)`>>
const min_test_4 : true = {} as Equal<'0',    Cion.Lisp<`(min 9 87 0 1)`>>
