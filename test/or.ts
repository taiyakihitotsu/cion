import type Cion from '../src/index.ts'

const maintest7_or: Cion.RawLisp<"(or true true)"> = ['prim', true]
const maintest8_or: Cion.RawLisp<"(or true false)"> = ['prim', true]
const maintest9_or: Cion.RawLisp<"(or false false)"> = ['prim', false]
const maintest10_or: Cion.RawLisp<"(or false true)"> = ['prim', true]
const maintest7_1_or: Cion.RawLisp<"(or true true true)"> = ['prim', true]
const maintest8_1_or: Cion.RawLisp<"(or false true false)"> = ['prim', true]
const maintest9_1_or: Cion.RawLisp<"(or false false false)"> = ['prim', false]
const maintest10_1_or: Cion.RawLisp<"(or false true true)"> = ['prim', true]
