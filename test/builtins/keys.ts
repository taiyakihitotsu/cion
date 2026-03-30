import type Cion from '../../src/index'
import type { Equal } from '../../src/util'

const keys_test_0 : true =
  {} as Equal<`[:a :b]`, Cion.Lisp<`(keys {:a 1 :b 2})`>>

const keys_test_1 : true =
  {} as Equal<`[]`, Cion.Lisp<`(keys {})`>>

const keys_test_2 : true =
  {} as Equal<'[0 1]', Cion.Lisp<`(keys ['x' 'y'])`>>

const keys_test_3 : true =
  {} as Equal<'[]', Cion.Lisp<`(keys [])`>>

const keys_test_4 : true =
  {} as Equal<`nil`, Cion.Lisp<`(keys 1)`>>
