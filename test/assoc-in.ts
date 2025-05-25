import type Cion from '../src/index'

const maintest0_assocIn_0: Cion.RawLisp<'(assoc-in [0 1 2] [0] 99)'> = ['vec', ['prim', '0000000001100011'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_assocIn_1: Cion.RawLisp<'(assoc-in [0 1 2] [99] 99)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']] // no effect
const maintest0_assocIn_2: Cion.RawLisp<'(assoc-in [0 1 2] [0 0] 99)'> = {error: "AssocInError8", message: "Keys rests but its value is not vector nor map."}
const maintest0_assocIn_3: Cion.RawLisp<'(assoc-in [0 1 [2 3 4]] [2 0] 99)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000001100011'], ['prim', '0000000000000011'], ['prim', '0000000000000100']]]
const maintest0_assocIn_4: Cion.RawLisp<'(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 0 :a] 99)'> = {error: 'AssocInError7', message: `The value of key (0000000000000000) is not vector nor map.`}
const maintest0_assocIn_5: Cion.RawLisp<'(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 2 :a] 99)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['map', [['key', ':a'], ['prim', '0000000001100011'], ['key', ':b'], ['prim', '0000000000000101']]]]]
const maintest0_assocIn_6: Cion.RawLisp<'(assoc-in {:x [0 1 [2 3 {:a 4 :b 5}]] :y 0} [:x 2 2 :a] 99)'> = ['map', [['key', ':x'], ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['map', [['key', ':a'], ['prim', '0000000001100011'], ['key', ':b'], ['prim', '0000000000000101']]]]], ['key', ':y'], ['prim', '0000000000000000']]]

