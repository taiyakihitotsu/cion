import type Cion from '../src/index.ts'

const maintest0_sub: Cion.RawLisp<"(- 4 1)"> = ['prim', "0000000000000011"]
const maintest1_sub: Cion.RawLisp<"(- 1 0)"> = ['prim', "0000000000000001"]
const maintest2_sub: Cion.RawLisp<"(- 0 0)"> = ['prim', "0000000000000000"]
const maintest3_sub: Cion.RawLisp<"(- 0 1)"> = ['prim', "1111111111111111"]
const maintest4_sub: Cion.RawLisp<"(- 10 1)"> = ['prim', "0000000000001001"]
const maintest5_sub: Cion.RawLisp<"(- 10 -9)"> = ['prim', "0000000000010011"]
