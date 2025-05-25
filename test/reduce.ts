import type Cion from '../src/index.ts'

const maintest0_reduce_0: Cion.RawLisp<'(reduce (fn [r i] (+ r i)) 0 [0 1 2 3 4 5])'> = ['prim', '0000000000001111']
const maintest0_reduce_1: Cion.RawLisp<'(let [f (fn [r i] (+ r i))] (reduce f 0 [0 1 2 3 4 5]))'> = ['prim', '0000000000001111']
