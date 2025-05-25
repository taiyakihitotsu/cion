import type Cion from '../src/index.ts'

const maintest11_lte_0: Cion.RawLisp<"(<= 15 14)"> = [`prim`, false]
const maintest11_lte_1: Cion.RawLisp<"(<= 15 14 13)"> = [`prim`, false]
const maintest11_lte_2: Cion.RawLisp<"(<= 15 15 14)"> = [`prim`, false]
const maintest11_lte_0_1: Cion.RawLisp<"(<= 14 15)"> = [`prim`, true]
const maintest11_lte_1_1: Cion.RawLisp<"(<= 13 14 15)"> = [`prim`, true]
const maintest11_lte_2_1: Cion.RawLisp<"(<= 15 15 15)"> = [`prim`, true]
const maintest11_lte_2_2: Cion.RawLisp<"(<= 14 15 15)"> = [`prim`, true]
const maintest11_lte_2_3: Cion.RawLisp<"(<= 14 14 15)"> = [`prim`, true]
