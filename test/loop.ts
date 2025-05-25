import type Cion from '../src/index.ts'

const maintest0_loop_1: Cion.RawLisp<'(let [f (fn [i] (if (> i 0) (f (- i 2)) true))] (f 10))'> = ['prim', true] 
