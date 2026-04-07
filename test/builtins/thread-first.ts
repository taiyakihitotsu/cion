import type Cion from '../../src/index.js'
import type { LispThreadFirst, ThreadFirst } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// --- ThreadFirst Internal Type Tests ---
const tf_int_0 : true = {} as Equal<[[4], [[3], [[2], [[1], [0]]]]], ThreadFirst<[0], [[[1]], [[2]], [[3]], [[4]]]>>
const tf_int_1 : true = {} as Equal<[[4], [[3], [[2], [[1], [0]], [22]]]], ThreadFirst<[0], [[[1]], [[2], [22]], [[3]], [[4]]]>>
const tf_int_2 : true = {} as Equal<[[4], [[3], [[2], [[1], [0]], [22]]]], ThreadFirst<[0], [[[1]], [[2], [22]], [3], [[4]]]>>
const tf_int_3 : true = {} as Equal<[[4], [[3], [[2], [[1], [[0]]]]]], ThreadFirst<[[0]], [[[1]], [[2]], [[3]], [[4]]]>>
const tf_int_4 : true = {} as Equal<[[4], [[3], [[2], [[1], [[0]]], [22]]]], ThreadFirst<[[0]], [[[1]], [[2], [22]], [[3]], [[4]]]>>
const tf_int_5 : true = {} as Equal<[[4], [[3], [[2], [[1], [[0]]], [22]]]], ThreadFirst<[[0]], [[[1]], [[2], [22]], [3], [[4]]]>>
const tf_int_6 : true = {} as Equal<[[1], [0]], ThreadFirst<[0], [[1]]>>

// --- LispThreadFirst Internal Type Tests ---
const ltf_int_0 : true = {} as Equal<[[4], [[3], [[2], [[1], [0]]]]], LispThreadFirst<[[0], [[1]], [[2]], [[3]], [[4]]]>>
const ltf_int_1 : true = {} as Equal<[[4], [[3], [[2], [[1], [0]], [22]]]], LispThreadFirst<[[0], [[1]], [[2], [22]], [[3]], [[4]]]>>
const ltf_int_2 : true = {} as Equal<[[4], [[3], [[2], [[1], [0]], [22]]]], LispThreadFirst<[[0], [[1]], [[2], [22]], [3], [[4]]]>>
const ltf_int_3 : true = {} as Equal<[[4], [[3], [[2], [[1], [[0]]]]]], LispThreadFirst<[[[0]], [[1]], [[2]], [[3]], [[4]]]>>
const ltf_int_4 : true = {} as Equal<[[4], [[3], [[2], [[1], [[0]]], [22]]]], LispThreadFirst<[[[0]], [[1]], [[2], [22]], [[3]], [[4]]]>>
const ltf_int_5 : true = {} as Equal<[[4], [[3], [[2], [[1], [[0]]], [22]]]], LispThreadFirst<[[[0]], [[1]], [[2], [22]], [3], [[4]]]>>
const ltf_int_6 : true = {} as Equal<[[1], [0]], LispThreadFirst<[[0], [1]]>>

// --- RawLisp Macro Expansion Tests ---
// String threading
const tf_raw_0 : true = {} as Equal<['prim', "'s01'"], Cion.RawLisp<"(-> 's' (str '01'))">>
const tf_raw_1 : true = {} as Equal<['prim', "'a01s'"], Cion.RawLisp<"(-> 'a' (str '01') (str 's'))">>
const tf_raw_2 : true = {} as Equal<['prim', "'a01s'"], Cion.RawLisp<"(str 'a' (str '01' 's'))">>

// Arithmetic threading (1 -> inc -> add 2)
const tf_raw_3 : true = {} as Equal<
  ['prim', ['0000000000000100', '0000000000000001']], 
  Cion.RawLisp<"(-> 1 (+ 1) (+ 2))">
>
const tf_raw_4 : true = {} as Equal<
  ['prim', ['0000000000000100', '0000000000000001']], 
  Cion.RawLisp<"(+ 2 (+ 1 1))">
>
