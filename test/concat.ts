import type Cion from '../src/index'

const maintest0_concat_0: Cion.RawLisp<'(concat [0 1] [2 3])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']]
const maintest0_concat_1: Cion.RawLisp<'(concat [0 1] [2] [3])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']]
const maintest0_concat_2: Cion.RawLisp<'(concat [] [0 1] [2] [3])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']]
