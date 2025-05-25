import type Cion from '../src/index.ts'

const maintest11_gte_0: Cion.RawLisp<"(>= 15 14)"> = [`prim`, true]
const maintest11_gte_1: Cion.RawLisp<"(>= 15 14 13)"> = [`prim`, true]
const maintest11_gte_2: Cion.RawLisp<"(>= 13 14 15)"> = [`prim`, false]
const maintest11_gte_0_1: Cion.RawLisp<"(>= 14 15)"> = [`prim`, false]
const maintest11_gte_1_1: Cion.RawLisp<"(>= 13 14 15)"> = [`prim`, false]
const maintest11_gte_2_1: Cion.RawLisp<"(>= 15 15 15)"> = [`prim`, true]
const maintest11_gte_2_2: Cion.RawLisp<"(>= 15 15 14)"> = [`prim`, true]
const maintest11_gte_2_3: Cion.RawLisp<"(>= 15 14 14)"> = [`prim`, true]
const maintest11_gte_2_4: Cion.RawLisp<"(>= 15 14 15)"> = [`prim`, false]
