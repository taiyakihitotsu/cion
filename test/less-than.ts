import type Cion from '../src/index'
import type { LispRelation } from '../src/index'

const testlisplt0: LispRelation<'<',  [[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, false]
const testlisplt1: LispRelation<'<',  [[`prim`, '00001111'], [`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, false]
const testlisplt2: LispRelation<'<',  [[`prim`, '00001111'], [`prim`, '00001111'], [`prim`, '00001111']]> = [`prim`, false]

const maintest11_lt_0: Cion.RawLisp<"(< 15 14)"> = [`prim`, false]
const maintest11_lt_1: Cion.RawLisp<"(< 15 14 13)"> = [`prim`, false]
const maintest11_lt_0_1: Cion.RawLisp<"(< 14 15)"> = [`prim`, true]
const maintest11_lt_1_1: Cion.RawLisp<"(< 13 14 15)"> = [`prim`, true]
const maintest11_lt_2: Cion.RawLisp<"(< 13 15 14)"> = [`prim`, false]
const maintest11_lt_3: Cion.RawLisp<"(< 15 14 13)"> = [`prim`, false]
