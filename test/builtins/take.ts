import type Cion from '../../src/index.js'
import type { Take } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// Internal Take type test (Binary string "11" equals 3)
const take_internal_test_0 : true = {} as Equal<
  [0, 1, 2], 
  Take<"11", [0, 1, 2, 3, 4, 5, 6]>
>

// RawLisp surface tests (AST level)
const take_raw_test_0 : true = {} as Equal<
  ['vec'], 
  Cion.RawLisp<'(take 0 [0 1 2 3 4 5])'>
>

const take_raw_test_1 : true = {} as Equal<
  ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001']], 
  Cion.RawLisp<'(take 2 [0 1 2 3 4 5])'>
>

const take_raw_test_2 : true = {} as Equal<
  ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']], 
  Cion.RawLisp<'(take 9 [0 1 2 3 4 5])'>
>

// Lisp (string result) tests
const take_lisp_test_0 : true = {} as Equal<'[]',            Cion.Lisp<`(take 0 [0 1 2 3 4 5 6])`>>
const take_lisp_test_1 : true = {} as Equal<'[0 1]',         Cion.Lisp<`(take 2 [0 1 2 3 4 5 6])`>>
const take_lisp_test_2 : true = {} as Equal<'[0 1 2 3 4 5 6]', Cion.Lisp<`(take 9 [0 1 2 3 4 5 6])`>>
