import type Cion from '../../src/index.js'
import type { Interleave } from '../../src/index.js'
import type {Equal} from '../../src/util.js'

const interleave_test_0 : true = {} as Equal<[1, 4, 2, 5, 3, 6]
,  Interleave<[1, 2, 3], [4, 5, 6]>>

const interleave_test_1 : true = {} as Equal<[1, 4, 2, 5]
,  Interleave<[1, 2, 3], [4, 5]>>

const interleave_test_2 : true = {} as Equal<[1, 2]
,  Interleave<[1], [2]>>

const interleave_test_3 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000000'], ['prim', '0000000000000001']], 
  Cion.RawLisp<'(interleave [0 0 0] [1 1 1])'>>
const interleave_test_4 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000000'], ['prim', '0000000000000001']],
  Cion.RawLisp<'(interleave [0 0 0] [1 1])'>>
