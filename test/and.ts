import type Cion from '../src/index.ts'

const maintest7_and: Cion.RawLisp<"(and true true)"> = ['prim', true]
const maintest8_and: Cion.RawLisp<"(and true false)"> = ['prim', false]
const maintest9_and: Cion.RawLisp<"(and false false)"> = ['prim', false]
const maintest10_and: Cion.RawLisp<"(and false true)"> = ['prim', false]
const maintest7_1_and: Cion.RawLisp<"(and true true true)"> = ['prim', true]
const maintest8_1_and: Cion.RawLisp<"(and false true false)"> = ['prim', false]
const maintest9_1_and: Cion.RawLisp<"(and false false false)"> = ['prim', false]
const maintest10_1_and: Cion.RawLisp<"(and false true true)"> = ['prim', false]
