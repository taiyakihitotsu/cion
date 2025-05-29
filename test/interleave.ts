import type Cion from '../src/index'
import type { Interleave } from '../src/index'

const testinterleave0: Interleave<[1, 2, 3], [4, 5, 6]> = [1, 4, 2, 5, 3, 6];
const testinterleave1: Interleave<[1, 2, 3], [4, 5]> = [1, 4, 2, 5];
const testinterleave2: Interleave<[1], [2]> = [1, 2];

const maintest0_interleave_0: Cion.RawLisp<'(interleave [0 0 0] [1 1 1])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000000'], ['prim', '0000000000000001']]
const maintest0_interleave_1: Cion.RawLisp<'(interleave [0 0 0] [1 1])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000000'], ['prim', '0000000000000001']]
