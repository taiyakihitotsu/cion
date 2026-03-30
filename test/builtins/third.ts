import type Cion from '../../src/index'
import type { Equal } from '../../src/util'

// Lisp (string result) tests for 'third'
const third_test_0 : true = {} as Equal<
  '2', 
  Cion.Lisp<`(third [0 1 2 3 4 5])`>>

const third_test_1 : true = {} as Equal<
  'nil', 
  Cion.Lisp<`(third [0])`>>

const third_test_2 : true = {} as Equal<
  'nil', 
  Cion.Lisp<`(third {:a 1})`>>
