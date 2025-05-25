import type Cion from '../src/index.ts'

const maintest0_not: Cion.RawLisp<"(not true)"> = ['prim', false]
const maintest1_not: Cion.RawLisp<"(not false)"> = ['prim', true]
