import type Cion from '../src/index'

const maintest0_update_0: Cion.RawLisp<'(update [0 1 2] 1 (fn [x] (+ x 99)))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000001100100'], ['prim', '0000000000000010']]
const maintest0_update_1: Cion.RawLisp<'(update [0 1 2] 99 (fn [x] (+ x 99)))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']] // no effect

