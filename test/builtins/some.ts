import type Cion from '../../src/index'
import type { Equal } from '../../src/util'

// Basic some behavior with closures
const some_test_0 : true = {} as Equal<'true', Cion.Lisp<`(some (fn [x] (number? x)) [0 1 2])`>>

// Using built-in predicate directly
const some_test_1 : true = {} as Equal<'true', Cion.Lisp<`(some number? [0 1 2])`>>

// Mixed types in vector
const some_test_2 : true = {} as Equal<'true', Cion.Lisp<`(some (fn [x] (number? x)) [0 '1' 2])`>>

// Empty collection and negative cases
const some_test_3 : true = {} as Equal<'false', Cion.Lisp<`(some number? [])`>>
const some_test_4 : true = {} as Equal<'false', Cion.Lisp<`(some number? ['1' '1'])`>>
