import type Cion from '../../src/index.ts'
import type { Equal } from '../../src/util'

// RawLisp surface tests (Remove elements < 3 from [0 1 2 3 4 5])
const remove_raw_test_0 : true = {} as Equal<
  ['vec', ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']],
  Cion.RawLisp<'(remove (fn [n] (> 3 n)) [0 1 2 3 4 5])'>
>

const remove_raw_test_1 : true = {} as Equal<
  ['vec', ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']],
  Cion.RawLisp<'(let [f (fn [n] (> 3 n))] (remove f [0 1 2 3 4 5]))'>
>

// Lisp (string result) tests using number? predicate
const remove_test_0 : true = {} as Equal<'[]', Cion.Lisp<`(remove number? [])`>>

const remove_test_1 : true = {} as Equal<
  "['s' false]",
  Cion.Lisp<`(remove number? ['s' 0 1 false 2])`>>

const remove_test_2 : true = {} as Equal<'[]', Cion.Lisp<`(remove number? [0 1 2])`>>

const remove_test_3 : true = {} as Equal<
  "[true false 's']",
  Cion.Lisp<`(remove number? [true false 's'])`>>
