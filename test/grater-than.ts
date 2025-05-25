import type Cion from '../src/index.ts'

const maintest11_gt_0: Cion.RawLisp<"(> 15 14)"> = [`prim`, true]
const maintest11_gt_1: Cion.RawLisp<"(> 15 14 13)"> = [`prim`, true]
const maintest11_gt_2: Cion.RawLisp<"(> 15 14 16)"> = [`prim`, false]
