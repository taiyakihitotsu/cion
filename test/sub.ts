import type Cion from '../src/index'
import type { LispSub } from '../src/index'
import type * as de from '../src/decimal'

const testlispsub0: LispSub<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, ['0000000000000010', '0000000000000001']]
const testlispsub1: LispSub<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, ['0000000000001011', '0000000000000001']]
// -- rational
const testlispsub2: LispSub<[[`prim`, de.DtoB<'15'>], [`prim`, ['0000001', '0000000000000010']], [`prim`, '00000011']]> = [`prim`, ['0000000000010111', '0000000000000010']] // 23


const maintest0_sub: Cion.RawLisp<"(- 4 1)"> = ['prim', ["0000000000000011", '0000000000000001']]
const maintest1_sub: Cion.RawLisp<"(- 1 0)"> = ['prim', ["0000000000000001", '0000000000000001']]
const maintest2_sub: Cion.RawLisp<"(- 0 0)"> = ['prim', ["0000000000000000", '0000000000000001']]
const maintest3_sub: Cion.RawLisp<"(- 0 1)"> = ['prim', ["1111111111111111", '0000000000000001']]
const maintest4_sub: Cion.RawLisp<"(- 10 1)"> = ['prim', ["0000000000001001", '0000000000000001']]
const maintest5_sub: Cion.RawLisp<"(- 10 -9)"> = ['prim', ["0000000000010011", '0000000000000001']]

const sub_test0 : Cion.Lisp<`(- 10 5)`> = '5'
const sub_test1 : Cion.Lisp<`(- 10 10)`> = '0'
const sub_test2 : Cion.Lisp<`(- 1 1)`> = '0'
const sub_test3 : Cion.Lisp<`(- 1 0)`> = '1'
const sub_test4 : Cion.Lisp<`(- 0 1)`> = '-1'
const sub_test5 : Cion.Lisp<`(- 0 0)`> = '0'
const sub_test6 : Cion.Lisp<`(- 5 10)`> = '-5'

const sub_test0a : Cion.Lisp<`(- 10/3 5/3)`> = '5/3'
const sub_test1a : Cion.Lisp<`(- 10/3 10/3)`> = '0'
const sub_test2a : Cion.Lisp<`(- 1/3 1/3)`> = '0'
const sub_test3a : Cion.Lisp<`(- 1/3 0)`> = '1/3'
const sub_test6a : Cion.Lisp<`(- 0 1/3)`> = '-1/3'
const sub_test4a : Cion.Lisp<`(- 0 0)`> = '0'
const sub_test5a : Cion.Lisp<`(- 5/3 10/3)`> = '-5/3'

const sub_test0b : Cion.Lisp<`(- 10/3 5/2)`> = '5/6'
const sub_test1b : Cion.Lisp<`(- 10/3 10/7)`> = '40/21'
const sub_test2b : Cion.Lisp<`(- 1/3 1/2)`> = '-1/6'
const sub_test3b : Cion.Lisp<`(- 1/2 1/3)`> = '1/6'
const sub_test4b : Cion.Lisp<`(- 5/2 10/3)`> = '-5/6'

const sub_test0c : Cion.Lisp<`(- 10/3 7)`> = '-11/3'
const sub_test1c : Cion.Lisp<`(- 1/3 7)`> = '-20/3'
const sub_test2c : Cion.Lisp<`(- 7 10/3)`> = '11/3'

const sub_test0d : Cion.Lisp<`(- 1)`> = '-1'
const sub_test1d : Cion.Lisp<`(- -1)`> = '1'
const sub_test2d : Cion.Lisp<`(- 2/3)`> = '-2/3'
const sub_test3d : Cion.Lisp<`(- -2/3)`> = '2/3'
const sub_test4d : Cion.Lisp<`(- 0)`> = '0'
