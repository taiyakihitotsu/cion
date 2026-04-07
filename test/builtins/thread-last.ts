import type Cion from '../../src/index.js'
import type { ThreadLast, LispThreadLast } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// --- LispThreadLast Internal Type Tests ---
const ltl_int_0 : true = {} as Equal<[[4], [[3], [[2], [[1], [0]]]]], LispThreadLast<[[0], [[1]], [[2]], [[3]], [[4]]]>>
const ltl_int_1 : true = {} as Equal<[[4], [[3], [[2], [22], [[1], [0]]]]], LispThreadLast<[[0], [[1]], [[2], [22]], [[3]], [[4]]]>>
const ltl_int_2 : true = {} as Equal<[[4], [[3], [[2], [22], [[1], [0]]]]], LispThreadLast<[[0], [[1]], [[2], [22]], [3], [[4]]]>>
const ltl_int_3 : true = {} as Equal<[[4], [[3], [[2], [[1], [[0]]]]]], LispThreadLast<[[[0]], [[1]], [[2]], [[3]], [[4]]]>>
const ltl_int_4 : true = {} as Equal<[[4], [[3], [[2], [22], [[1], [[0]]]]]], LispThreadLast<[[[0]], [[1]], [[2], [22]], [[3]], [[4]]]>>
const ltl_int_5 : true = {} as Equal<[[4], [[3], [[2], [22], [[1], [[0]]]]]], LispThreadLast<[[[0]], [[1]], [[2], [22]], [3], [[4]]]>>
const ltl_int_6 : true = {} as Equal<[[1], [0]], LispThreadLast<[[0], [1]]>>

// --- ThreadLast Internal Type Tests ---
const tl_int_0 : true = {} as Equal<[[4], [[3], [[2], [[1], [0]]]]], ThreadLast<[0], [[[1]], [[2]], [[3]], [[4]]]>>
const tl_int_1 : true = {} as Equal<[[4], [[3], [[2], [22], [[1], [0]]]]], ThreadLast<[0], [[[1]], [[2], [22]], [[3]], [[4]]]>>
const tl_int_2 : true = {} as Equal<[[4], [[3], [[2], [22], [[1], [0]]]]], ThreadLast<[0], [[[1]], [[2], [22]], [3], [[4]]]>>
const tl_int_3 : true = {} as Equal<[[4], [[3], [[2], [[1], [[0]]]]]], ThreadLast<[[0]], [[[1]], [[2]], [[3]], [[4]]]>>
const tl_int_4 : true = {} as Equal<[[4], [[3], [[2], [22], [[1], [[0]]]]]], ThreadLast<[[0]], [[[1]], [[2], [22]], [[3]], [[4]]]>>
const tl_int_5 : true = {} as Equal<[[4], [[3], [[2], [22], [[1], [[0]]]]]], ThreadLast<[[0]], [[[1]], [[2], [22]], [3], [[4]]]>>

// --- RawLisp Macro Expansion Tests ---
// String threading (Thread-Last inserts at the end of 'str' arguments)
const tl_raw_0 : true = {} as Equal<['prim', "'01s'"], Cion.RawLisp<"(->> 's' (str '01'))">>
const tl_raw_1 : true = {} as Equal<['prim', "'s01a'"], Cion.RawLisp<"(->> 'a' (str '01') (str 's'))">>
const tl_raw_2 : true = {} as Equal<['prim', "'a01s'"], Cion.RawLisp<"(str 'a' (str '01' 's'))">>

// Arithmetic threading (1 -> (+ 1) -> (+ 2) => (+ 2 (+ 1 1)) = 4)
const tl_raw_3 : true = {} as Equal<
  ['prim', ['0000000000000100', '0000000000000001']], 
  Cion.RawLisp<"(->> 1 (+ 1) (+ 2))">
>
const tl_raw_4 : true = {} as Equal<
  ['prim', ['0000000000000100', '0000000000000001']], 
  Cion.RawLisp<"(+ 2 (+ 1 1))">
>
