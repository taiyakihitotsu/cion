import type Cion from '../../src/index'
import type { Reduce } from '../../src/index'
import type { Equal } from '../../src/util'

// Internal Reduce type test
const reduce_internal_test_0 : true = {} as Equal<
  ['prim', ['0000000000000011', '0000000000000001']],
  Reduce<
    ['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]],
    ['prim', '0'],
    ['vec', ['prim', '01'], ['prim', '10']]
  >
>

// RawLisp surface tests (Sum of 0..5 = 15)
const reduce_raw_test_0 : true = {} as Equal<
  ['prim', ['0000000000001111', '0000000000000001']],
  Cion.RawLisp<'(reduce (fn [r i] (+ r i)) 0 [0 1 2 3 4 5])'>
>

const reduce_raw_test_1 : true = {} as Equal<
  ['prim', ['0000000000001111', '0000000000000001']],
  Cion.RawLisp<'(let [f (fn [r i] (+ r i))] (reduce f 0 [0 1 2 3 4 5]))'>
>
