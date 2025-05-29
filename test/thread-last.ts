import type Cion from '../src/index'
import type { ThreadLast, LispThreadLast } from '../src/index'

const lispthreadlasttest0: LispThreadLast<[[0], [[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]]]]]
const lispthreadlasttest1: LispThreadLast<[[0], [[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [22], [[1], [0]]]]]
const lispthreadlasttest2: LispThreadLast<[[0], [[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [22],  [[1], [0]]]]]
const lispthreadlasttest3: LispThreadLast<[[[0]], [[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]]]]]
const lispthreadlasttest4: LispThreadLast<[[[0]], [[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [22], [[1], [[0]]]]]]
const lispthreadlasttest5: LispThreadLast<[[[0]], [[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [22], [[1], [[0]]]]]]
const lispthreadlasttest6: LispThreadLast<[[0], [1]]> = [[1], [0]]

const threadlasttest0: ThreadLast<[0], [[[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]]]]]
const threadlasttest1: ThreadLast<[0], [[[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [22],[[1],[0]]]]]
const threadlasttest2: ThreadLast<[0], [[[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [22], [[1], [0]]]]]
const threadlasttest3: ThreadLast<[[0]], [[[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]]]]]
const threadlasttest4: ThreadLast<[[0]], [[[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2],[22],[[1], [[0]]]]]]
const threadlasttest5: ThreadLast<[[0]], [[[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2],[22], [[1], [[0]]]]]]

const maintest_threadl_0: Cion.RawLisp<"(->> 's' (str '01'))"> = ['prim', "'01s'"]
const maintest_threadl_1: Cion.RawLisp<"(->> 'a' (str '01') (str 's'))"> = ['prim', "'s01a'"]
const maintest_threadl_2: Cion.RawLisp<"(str 'a' (str '01' 's'))"> = ['prim', "'a01s'"]
const maintest_threadl_3: Cion.RawLisp<"(->> 1 (+ 1) (+ 2))"> = ['prim', '0000000000000100']
const maintest_threadl_4: Cion.RawLisp<"(+ 2 (+ 1 1))"> = ['prim', '0000000000000100']
