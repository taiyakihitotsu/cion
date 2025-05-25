import type Cion from '../src/index.ts'

const maintest_threadl_0: Cion.RawLisp<"(->> 's' (str '01'))"> = ['prim', "'01s'"]
const maintest_threadl_1: Cion.RawLisp<"(->> 'a' (str '01') (str 's'))"> = ['prim', "'s01a'"]
const maintest_threadl_2: Cion.RawLisp<"(str 'a' (str '01' 's'))"> = ['prim', "'a01s'"]
const maintest_threadl_3: Cion.RawLisp<"(->> 1 (+ 1) (+ 2))"> = ['prim', '0000000000000100']
const maintest_threadl_4: Cion.RawLisp<"(+ 2 (+ 1 1))"> = ['prim', '0000000000000100']
