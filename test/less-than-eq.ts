import type Cion from '../src/index'
import type { LispRelation } from '../src/index'

const testlisplte0: LispRelation<'<=',  [[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, false]
const testlisplte1: LispRelation<'<=',  [[`prim`, '00001111'], [`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, false]
const testlisplte2: LispRelation<'<=',  [[`prim`, '00001111'], [`prim`, '00001111'], [`prim`, '00001111']]> = [`prim`, true]

const maintest11_lte_0: Cion.RawLisp<"(<= 15 14)"> = [`prim`, false]
const maintest11_lte_1: Cion.RawLisp<"(<= 15 14 13)"> = [`prim`, false]
const maintest11_lte_2: Cion.RawLisp<"(<= 15 15 14)"> = [`prim`, false]
const maintest11_lte_0_1: Cion.RawLisp<"(<= 14 15)"> = [`prim`, true]
const maintest11_lte_1_1: Cion.RawLisp<"(<= 13 14 15)"> = [`prim`, true]
const maintest11_lte_2_1: Cion.RawLisp<"(<= 15 15 15)"> = [`prim`, true]
const maintest11_lte_2_2: Cion.RawLisp<"(<= 14 15 15)"> = [`prim`, true]
const maintest11_lte_2_3: Cion.RawLisp<"(<= 14 14 15)"> = [`prim`, true]
