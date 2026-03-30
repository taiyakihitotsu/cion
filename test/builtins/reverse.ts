import type Cion from '../../src/index'
import type { Reverse } from '../../src/index'
import type { Equal } from '../../src/util'

// Internal Reverse type tests (Tuple level)
const reverse_internal_test_0 : true = {} as Equal<[4, 3, 2, 1, 0], Reverse<[0, 1, 2, 3, 4]>>
const reverse_internal_test_1 : true = {} as Equal<[],              Reverse<[]>>
const reverse_internal_test_2 : true = {} as Equal<[1],             Reverse<[1]>>
const reverse_internal_test_3 : true = {} as Equal<[[1, 2]],        Reverse<[[1, 2]]>>

// RawLisp surface tests (AST level)
const reverse_raw_test_0 : true = {} as Equal<
  ['vec', ['prim', '0000000000000011'], ['prim', '0000000000000010'], ['prim', '0000000000000001'], ['prim', '0000000000000000']],
  Cion.RawLisp<'(reverse [0 1 2 3])'>
>

const reverse_raw_test_1 : true = {} as Equal<
  ['vec', ['prim', '0000000000000000']],
  Cion.RawLisp<'(reverse [0])'>
>
