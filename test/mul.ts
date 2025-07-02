import type Cion from '../src/index'
import type { LispMul } from '../src/index'
import * as de from '../src/decimal'

const testlispmul0: LispMul<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, ['0000000000000011', '0000000000000001']]
const testlispmul1: LispMul<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, ['0000000000101101', '0000000000000001']]
// -- relational
const testlispmul2: LispMul<[[`prim`, '00001111'], [`prim`, ['0000001', de.DtoB<'2'>]], [`prim`, '00000011']]> = [`prim`, ['0000000000101101', '0000000000000010']]


const maintest0_mul: Cion.RawLisp<"(* 3 1)"> = ['prim', ["0000000000000011", '0000000000000001']]
const maintest1_mul: Cion.RawLisp<"(* 0 1)"> = ['prim', ["0000000000000000", '0000000000000001']]
const maintest2_mul: Cion.RawLisp<"(* 0 0)"> = ['prim', ["0000000000000000", '0000000000000001']]
const maintest3_mul: Cion.RawLisp<"(* 2 3)"> = ['prim', ["0000000000000110", '0000000000000001']]
const maintest4_mul: Cion.RawLisp<"(* (- 2 3) 3)"> = ['prim', ["1111111111111101", '0000000000000001']]
const maintest5_mul: Cion.RawLisp<"(* (- 0 2) 3)"> = ['prim', ["1111111111111010", '0000000000000001']]
const maintest6_mul: Cion.RawLisp<"(* -2 3)"> = ['prim', ["1111111111111010", '0000000000000001']]

const mul_test0 : Cion.Lisp<`(* 10 5)`> = '50'
const mul_test1 : Cion.Lisp<`(* 10 10)`> = '100'
const mul_test2 : Cion.Lisp<`(* 1 1)`> = '1'
const mul_test3 : Cion.Lisp<`(* 1 0)`> = '0'
const mul_test4 : Cion.Lisp<`(* 0 1)`> = '0'
const mul_test5 : Cion.Lisp<`(* 0 0)`> = '0'
const mul_test6 : Cion.Lisp<`(* 5 10)`> = '50'

const mul_test0a : Cion.Lisp<`(* 10/3 5/3)`> = '50/9'
const mul_test1a : Cion.Lisp<`(* 10/3 10/3)`> = '100/9'
const mul_test2a : Cion.Lisp<`(* 1/3 1/3)`> = '1/9'
const mul_test3a : Cion.Lisp<`(* 1/3 0)`> = '0'
const mul_test6a : Cion.Lisp<`(* 0 1/3)`> = '0'
const mul_test4a : Cion.Lisp<`(* 0 0)`> = '0'
const mul_test5a : Cion.Lisp<`(* 5/3 10/3)`> = '50/9'

const mul_test0b : Cion.Lisp<`(* 10/3 5/2)`> = '25/3'
const mul_test1b : Cion.Lisp<`(* 10/3 10/7)`> = '100/21'
const mul_test2b : Cion.Lisp<`(* 1/3 1/2)`> = '1/6'
const mul_test3b : Cion.Lisp<`(* 1/2 1/3)`> = '1/6'
const mul_test4b : Cion.Lisp<`(* 5/2 10/3)`> = '25/3'

const mul_test0c : Cion.Lisp<`(* 10/3 7)`> = '70/3'
const mul_test1c : Cion.Lisp<`(* 1/3 7)`> = '7/3'
const mul_test2c : Cion.Lisp<`(* 7 10/3)`> = '70/3'

const mul_test_f0 : Cion.Lisp<`(* 1 0)`> = '0'
const mul_test_f1 : Cion.Lisp<`(* 1 nil)`>['ast']['error'] = 'LispMulError1'
const mul_est_f2 : Cion.Lisp<`(* 1 true)`>['ast']['error'] = 'LispMulError1'
const mul_test_f3 : Cion.Lisp<`(* 1 'string')`>['ast']['error'] = 'LispMulError1'
const mul_test_f4 : Cion.Lisp<`(* 1 mul)`>['ast']['error'] = 'LispMulError1'
const mul_test_f5 : Cion.Lisp<`(* 1 (fn [x] x))`>['ast']['error'] = 'LispMulError1'
const mul_test_f6 : Cion.Lisp<`(* 1 [])`>['ast']['error'] = 'LispMulError1'
const mul_test_f7 : Cion.Lisp<`(* 1 {})`>['ast']['error'] = 'LispMulError1'

const mul_test_f8 : Cion.Lisp<`(* 1 2/3)`> = '2/3'
const mul_test_f : Cion.Lisp<`(* 1 -2/3)`> = '-2/3'
