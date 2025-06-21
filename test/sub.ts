import type Cion from '../src/index'
import type { LispSub } from '../src/index'
import type * as de from '../src/decimal'

const testlispsub0: LispSub<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, ['0000000000000010', '0000000000000001']]
const testlispsub1: LispSub<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, ['0000000000001011', '0000000000000001']]
// -- rational
const testlispsub2: LispSub<[[`prim`, de.DtoB<'15'>], [`prim`, ['0000001', '0000000000000010']], [`prim`, '00000011']]> = [`prim`, ['0000000000010111', '0000000000000010']] // 23


const maintest0_sub: Cion.RawLisp<"(- 4 1)"> = ['prim', ["0000000000000011", '0000000000000001']]
const maintest1_sub: Cion.RawLisp<"(- 1 0)"> = ['prim', ["0000000000000001", '0000000000000001']]
const maintest2_sub: Cion.RawLisp<"(- 0 0)"> = ['prim', ["0000000000000000", '0000000000000001']]
const maintest3_sub: Cion.RawLisp<"(- 0 1)"> = ['prim', ["1111111111111111", '0000000000000001']]
const maintest4_sub: Cion.RawLisp<"(- 10 1)"> = ['prim', ["0000000000001001", '0000000000000001']]
const maintest5_sub: Cion.RawLisp<"(- 10 -9)"> = ['prim', ["0000000000010011", '0000000000000001']]
