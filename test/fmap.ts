import type Cion from '../src/index.ts'

const maintest0_map_0: Cion.RawLisp<'(map (fn [n] (* 2 n)) [0 1 2])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000010'], ['prim', '0000000000000100']]
const maintest0_map_1: Cion.RawLisp<'(let [f (fn [n] (* 2 n))] (map f [0 1 2]))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000010'], ['prim', '0000000000000100']]
