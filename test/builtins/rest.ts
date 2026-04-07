import type Cion from '../../src/index.js'
import type { Rest } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// Internal Rest type tests
type testvec = [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1]];

const rest_internal_test_0 : true = {} as Equal<[`vec`, [`prim`, 0], [`prim`, 1]], Rest<testvec>>
const rest_internal_test_1 : true = {} as Equal<[`vec`, [`prim`, 1]], Rest<Rest<testvec>>>
const rest_internal_test_2 : true = {} as Equal<[`vec`], Rest<Rest<Rest<testvec>>>>

// RawLisp surface tests (Binary representation)
const rest_raw_test_0 : true = {} as Equal<
  ['vec', ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']],
  Cion.RawLisp<'(rest [0 1 2 3])'>
>

const rest_raw_test_1 : true = {} as Equal<['vec'], Cion.RawLisp<'(rest [0])'>>

// Lisp (string result) tests
const rest_test_0 : true = {} as Equal<'[1 2]', Cion.Lisp<`(rest [0 1 2])`>>
const rest_test_1 : true = {} as Equal<'[]',    Cion.Lisp<`(rest [0])`>>
const rest_test_2 : true = {} as Equal<'nil',   Cion.Lisp<`(rest [])`>>
