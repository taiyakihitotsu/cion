import type Cion from '../../src/index.js'
import type { Conj } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

type testvec = [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1]];

const conj_test_0 : true = {} as Equal<[
  `vec`,
  [`prim`, true],
  [`prim`, 0],
  [`prim`, 1],
  [`prim`, false],
], Conj<testvec, [`prim`, false]>>
const conj_test_1 : true = {} as Equal<[`vec`, [`prim`, false]], Conj<[`vec`], [`prim`, false]>>
const conj_test_2 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']], Cion.RawLisp<'(conj [0 1] 2 3)'>>
const conj_test_3 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010']]], Cion.RawLisp<'(conj [0 1] [2])'>>
