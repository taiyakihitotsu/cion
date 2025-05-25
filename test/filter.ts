import type Cion from '../src/index.ts'

const maintest0_filter_0: Cion.RawLisp<'(filter (fn [n] (> 3 n)) [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_filter_1: Cion.RawLisp<'(let [f (fn [n] (> 3 n))] (filter f [0 1 2 3 4 5]))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
