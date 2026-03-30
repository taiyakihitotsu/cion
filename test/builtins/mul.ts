import type Cion from '../../src/index'
import type { LispMul } from '../../src/index'
import * as de from '../../src/decimal'
import type { Equal } from '../../src/util'

const mul_internal_test_0 : true = {} as Equal<['prim', ['0000000000000011', '0000000000000001']], LispMul<[[`prim`, '00000011'], [`prim`, '0000001']]>>
const mul_internal_test_1 : true = {} as Equal<['prim', ['0000000000101101', '0000000000000001']], LispMul<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]>>
const mul_internal_test_2 : true = {} as Equal<['prim', ['0000000000101101', '0000000000000010']], LispMul<[[`prim`, '00001111'], [`prim`, ['0000001', de.DtoB<'2'>]], [`prim`, '00000011']]>>

const mul_raw_test_0 : true = {} as Equal<['prim', ["0000000000000011", '0000000000000001']], Cion.RawLisp<"(* 3 1)">>
const mul_raw_test_1 : true = {} as Equal<['prim', ["0000000000000000", '0000000000000001']], Cion.RawLisp<"(* 0 1)">>
const mul_raw_test_2 : true = {} as Equal<['prim', ["0000000000000000", '0000000000000001']], Cion.RawLisp<"(* 0 0)">>
const mul_raw_test_3 : true = {} as Equal<['prim', ["0000000000000110", '0000000000000001']], Cion.RawLisp<"(* 2 3)">>
const mul_raw_test_4 : true = {} as Equal<['prim', ["1111111111111101", '0000000000000001']], Cion.RawLisp<"(* (- 2 3) 3)">>
const mul_raw_test_5 : true = {} as Equal<['prim', ["1111111111111010", '0000000000000001']], Cion.RawLisp<"(* (- 0 2) 3)">>
const mul_raw_test_6 : true = {} as Equal<['prim', ["1111111111111010", '0000000000000001']], Cion.RawLisp<"(* -2 3)">>

const mul_test_0 : true = {} as Equal<'50',  Cion.Lisp<`(* 10 5)`>>
const mul_test_1 : true = {} as Equal<'100', Cion.Lisp<`(* 10 10)`>>
const mul_test_2 : true = {} as Equal<'1',   Cion.Lisp<`(* 1 1)`>>
const mul_test_3 : true = {} as Equal<'0',   Cion.Lisp<`(* 1 0)`>>

const mul_rational_test_0 : true = {} as Equal<'50/9',  Cion.Lisp<`(* 10/3 5/3)`>>
const mul_rational_test_1 : true = {} as Equal<'100/9', Cion.Lisp<`(* 10/3 10/3)`>>
const mul_rational_test_2 : true = {} as Equal<'1/9',   Cion.Lisp<`(* 1/3 1/3)`>>

const mul_rational_test_3 : true = {} as Equal<'25/3',   Cion.Lisp<`(* 10/3 5/2)`>>
const mul_rational_test_4 : true = {} as Equal<'100/21', Cion.Lisp<`(* 10/3 10/7)`>>
const mul_rational_test_5 : true = {} as Equal<'1/6',    Cion.Lisp<`(* 1/3 1/2)`>>

const mul_mixed_test_0 : true = {} as Equal<'70/3', Cion.Lisp<`(* 10/3 7)`>>
const mul_mixed_test_1 : true = {} as Equal<'7/3',  Cion.Lisp<`(* 1/3 7)`>>

const mul_signed_test_0 : true = {} as Equal<'2/3',  Cion.Lisp<`(* 1 2/3)`>>
const mul_signed_test_1 : true = {} as Equal<'-2/3', Cion.Lisp<`(* 1 -2/3)`>>

type GetError<T> = T extends { ast: { error: infer E } } ? E : never

const mul_error_test_0 : true = {} as Equal<'LispMulError1', GetError<Cion.Lisp<`(* 1 nil)`>>>
const mul_error_test_1 : true = {} as Equal<'LispMulError1', GetError<Cion.Lisp<`(* 1 true)`>>>
const mul_error_test_2 : true = {} as Equal<'LispMulError1', GetError<Cion.Lisp<`(* 1 'string')`>>>
const mul_error_test_3 : true = {} as Equal<'LispMulError1', GetError<Cion.Lisp<`(* 1 (fn [x] x))`>>>
