import type Cion from '../../src/index.js'
import { VNil } from '../../src/sexprtypes.js'
import type { Equal } from '../../src/util.js'

// RawLisp surface tests (Binary representation & VNil)
const second_raw_test_0 : true = {} as Equal<
  ['prim', '0000000000000001'], 
  Cion.RawLisp<'(second [0 1 2])'>
>

const second_raw_test_1 : true = {} as Equal<
  typeof VNil, 
  Cion.RawLisp<'(second [])'>
>

// Lisp (string result) tests
const second_test_0 : true = {} as Equal<'1',   Cion.Lisp<'(second [0 1 2])'>>
const second_test_1 : true = {} as Equal<'nil', Cion.Lisp<'(second [])'>>

// @ts-expect-error:
const maintest1_second_2: Cion.Lisp<'(second {:a 1})'> = 'nil'
