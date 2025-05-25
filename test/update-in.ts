import type Cion from '../src/index'

const maintest0_updateIn_0: Cion.RawLisp<'(update-in [0 1 2] [1] (fn [x] (+ x 99)))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000001100100'], ['prim', '0000000000000010']]
const maintest0_updateIn_1: Cion.RawLisp<'(update-in [0 1 2] [99] (fn [x] (+ x 99)))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']] // no effect
const maintest0_updateIn_2: Cion.RawLisp<'(update-in [0 1 2] [99 99] (fn [x] (+ x 99)))'> = {error: "AssocInError8", message: "Keys rests but its value is not vector nor map."}

