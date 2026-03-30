import type Cion from '../../src/index'
import type { LispNot } from '../../src/index'
import type { Equal } from '../../src/util'

const not_internal_test_0 : true = {} as Equal<['prim', true],  LispNot<[['prim', false]]>>
const not_internal_test_1 : true = {} as Equal<['prim', false], LispNot<[['prim', true]]>>
const not_internal_test_2 : true = {} as Equal<['prim', false], LispNot<[['prim', 1]]>>

const not_raw_test_0 : true = {} as Equal<['prim', false], Cion.RawLisp<"(not true)">>
const not_raw_test_1 : true = {} as Equal<['prim', true],  Cion.RawLisp<"(not false)">>
