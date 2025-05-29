import type Cion from '../src/index'
import type { LispNot } from '../src/index'

const lispnottest0: LispNot<[['prim', false]]> = ['prim', true]
const lispnottest1: LispNot<[['prim', true]]> = ['prim', false]
const lispnottest2: LispNot<[['prim', 1]]> = ['prim', false]

const maintest0_not: Cion.RawLisp<"(not true)"> = ['prim', false]
const maintest1_not: Cion.RawLisp<"(not false)"> = ['prim', true]
