import type Cion from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const fmap_test_0 : true = {} as Equal<['vec', ['prim', ['0000000000000000', '0000000000000001']], ['prim', ['0000000000000010', '0000000000000001']], ['prim', ['0000000000000100', '0000000000000001']]],
Cion.RawLisp<'(map (fn [n] (* 2 n)) [0 1 2])'>>
const fmap_test_1 : true = {} as Equal<['vec', ['prim', ['0000000000000000', '0000000000000001']], ['prim', ['0000000000000010', '0000000000000001']], ['prim', ['0000000000000100', '0000000000000001']]],
Cion.RawLisp<'(let [f (fn [n] (* 2 n))] (map f [0 1 2]))'>>
