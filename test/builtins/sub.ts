import type Cion from '../../src/index'
import type { LispSub } from '../../src/index'
import type * as de from '../../src/decimal'
import type { Equal } from '../../src/util'

// Internal LispSub type tests (Binary representation)
const sub_internal_test_0 : true = {} as Equal<[`prim`, ['0000000000000010', '0000000000000001']], LispSub<[[`prim`, '00000011'], [`prim`, '0000001']]>>
const sub_internal_test_1 : true = {} as Equal<[`prim`, ['0000000000001011', '0000000000000001']], LispSub<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]>>
const sub_internal_test_2 : true = {} as Equal<[`prim`, ['0000000000010111', '0000000000000010']], LispSub<[[`prim`, de.DtoB<'15'>], [`prim`, ['0000001', '0000000000000010']], [`prim`, '00000011']]>>

// RawLisp surface tests
const sub_raw_test_0 : true = {} as Equal<['prim', ["0000000000000011", '0000000000000001']], Cion.RawLisp<"(- 4 1)">>
const sub_raw_test_1 : true = {} as Equal<['prim', ["1111111111111111", '0000000000000001']], Cion.RawLisp<"(- 0 1)">>
const sub_raw_test_2 : true = {} as Equal<['prim', ["0000000000010011", '0000000000000001']], Cion.RawLisp<"(- 10 -9)">>

// Lisp (string result) tests - Integers
const sub_lisp_test_0 : true = {} as Equal<'5',  Cion.Lisp<`(- 10 5)`>>
const sub_lisp_test_1 : true = {} as Equal<'0',  Cion.Lisp<`(- 10 10)`>>
const sub_lisp_test_2 : true = {} as Equal<'-1', Cion.Lisp<`(- 0 1)`>>

// Lisp (string result) tests - Rationals
const sub_rat_test_0 : true = {} as Equal<'5/3',   Cion.Lisp<`(- 10/3 5/3)`>>
const sub_rat_test_1 : true = {} as Equal<'-5/3',  Cion.Lisp<`(- 5/3 10/3)`>>
const sub_rat_test_2 : true = {} as Equal<'5/6',   Cion.Lisp<`(- 10/3 5/2)`>>
const sub_rat_test_3 : true = {} as Equal<'-1/6',  Cion.Lisp<`(- 1/3 1/2)`>>
const sub_rat_test_4 : true = {} as Equal<'-11/3', Cion.Lisp<`(- 10/3 7)`>>

// Unary negation
const sub_unary_test_0 : true = {} as Equal<'-1',   Cion.Lisp<`(- 1)`>>
const sub_unary_test_1 : true = {} as Equal<'1',    Cion.Lisp<`(- -1)`>>
const sub_unary_test_2 : true = {} as Equal<'-2/3', Cion.Lisp<`(- 2/3)`>>

// Error Handling
type GetError<T> = T extends { ast: { error: infer E } } ? E : never

const sub_error_test_0 : true = {} as Equal<'LispSubError1', GetError<Cion.Lisp<`(- 1 nil)`>>>
const sub_error_test_1 : true = {} as Equal<'LispSubError0', GetError<Cion.Lisp<`(- 1 true)`>>>
const sub_error_test_2 : true = {} as Equal<'LispSubError1', GetError<Cion.Lisp<`(- 1 'string')`>>>
const sub_error_test_3 : true = {} as Equal<'LispSubError0', GetError<Cion.Lisp<`(- 1 [])`>>>

// Final checks
const sub_final_test_0 : true = {} as Equal<'1/3', Cion.Lisp<`(- 1 2/3)`>>
const sub_final_test_1 : true = {} as Equal<'5/3', Cion.Lisp<`(- 1 -2/3)`>>
