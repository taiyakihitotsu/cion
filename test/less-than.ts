import type Cion from '../src/index.ts'

const maintest11_lt_0: Cion.RawLisp<"(< 15 14)"> = [`prim`, false]
const maintest11_lt_1: Cion.RawLisp<"(< 15 14 13)"> = [`prim`, false]
const maintest11_lt_0_1: Cion.RawLisp<"(< 14 15)"> = [`prim`, true]
const maintest11_lt_1_1: Cion.RawLisp<"(< 13 14 15)"> = [`prim`, true]
const maintest11_lt_2: Cion.RawLisp<"(< 13 15 14)"> = [`prim`, false]
const maintest11_lt_3: Cion.RawLisp<"(< 15 14 13)"> = [`prim`, false]
