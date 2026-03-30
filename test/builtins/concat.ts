import type Cion from '../../src/index'
import type { Concat } from '../../src/index'
import type { Equal } from '../../src/util'

type testvec = [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1]];

const concat_test_0 : true = {} as Equal<[
  `vec`,
  [`prim`, true],
  [`prim`, 0],
  [`prim`, 1],
  [`prim`, true],
  [`prim`, 0],
  [`prim`, 1],
], Concat<testvec, testvec>>

const concat_test_1 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']], Cion.RawLisp<'(concat [0 1] [2 3])'>>

const concat_test_2 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']], Cion.RawLisp<'(concat [0 1] [2] [3])'>>

const concat_test_3 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']], Cion.RawLisp<'(concat [] [0 1] [2] [3])'>>
