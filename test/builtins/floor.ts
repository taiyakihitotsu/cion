import type Cion from '../../src/index.js'
import type {Equal} from '../../src/util.js'

const floor_test_0 : true = {} as Equal<'0', Cion.Lisp<`(floor 0)`>>
const floor_test_1 : true = {} as Equal<'2', Cion.Lisp<`(floor 2)`>>
const floor_test_2 : true = {} as Equal<'-2', Cion.Lisp<`(floor -2)`>>
const floor_test_3 : true = {} as Equal<'1', Cion.Lisp<`(floor 3/2)`>>
const floor_test_4 : true = {} as Equal<'-2', Cion.Lisp<`(floor -3/2)`>>
const floor_test_5 : true = {} as Equal<'2', Cion.Lisp<`(floor 7/3)`>>
const floor_test_6 : true = {} as Equal<'-3', Cion.Lisp<`(floor -7/3)`>>
const floor_test_7 : true = {} as Equal<'0', Cion.Lisp<`(floor 1/2)`>>
const floor_test_8 : true = {} as Equal<'-1', Cion.Lisp<`(floor -1/2)`>>
