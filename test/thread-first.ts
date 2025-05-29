import type Cion from '../src/index'
import type { LispThreadFirst, ThreadFirst } from '../src/index'

const threadfirsttest0: ThreadFirst<[0], [[[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]]]]]
const threadfirsttest1: ThreadFirst<[0], [[[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]], [22]]]]
const threadfirsttest2: ThreadFirst<[0], [[[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [[1], [0]], [22]]]]
const threadfirsttest3: ThreadFirst<[[0]], [[[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]]]]]
const threadfirsttest4: ThreadFirst<[[0]], [[[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]], [22]]]]
const threadfirsttest5: ThreadFirst<[[0]], [[[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]], [22]]]]
const threadfirsttest6: ThreadFirst<[0], [[1]]> = [[1], [0]]

const lispthreadfirsttest0: LispThreadFirst<[[0], [[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]]]]]
const lispthreadfirsttest1: LispThreadFirst<[[0], [[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]], [22]]]]
const lispthreadfirsttest2: LispThreadFirst<[[0], [[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [[1], [0]], [22]]]]
const lispthreadfirsttest3: LispThreadFirst<[[[0]], [[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]]]]]
const lispthreadfirsttest4: LispThreadFirst<[[[0]], [[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]], [22]]]]
const lispthreadfirsttest5: LispThreadFirst<[[[0]], [[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]], [22]]]]
const lispthreadfirsttest6: LispThreadFirst<[[0], [1]]> = [[1], [0]]

const maintest_threadf_0: Cion.RawLisp<"(-> 's' (str '01'))"> = ['prim', "'s01'"]
const maintest_threadf_1: Cion.RawLisp<"(-> 'a' (str '01') (str 's'))"> = ['prim', "'a01s'"]
const maintest_threadf_2: Cion.RawLisp<"(str 'a' (str '01' 's'))"> = ['prim', "'a01s'"]
const maintest_threadf_3: Cion.RawLisp<"(-> 1 (+ 1) (+ 2))"> = ['prim', '0000000000000100']
const maintest_threadf_4: Cion.RawLisp<"(+ 2 (+ 1 1))"> = ['prim', '0000000000000100'
]
