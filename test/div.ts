import type Cion from '../src/index'
import type { LispDiv } from '../src/index'

const testlispdiv0: LispDiv<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, ['0000000000000011', '0000000000000001']]
const testlispdiv1: LispDiv<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, ['0000000000001111', '0000000000000011']]
const testlispdiv2: LispDiv<[[`prim`, '00000011'], [`prim`, '0000000']]> = [`prim`, 'nil']

const maintest0_div: Cion.RawLisp<"(/ 4 1)"> = ['prim', ["0000000000000100", '0000000000000001']]
const maintest1_div: Cion.RawLisp<"(/ 1 0)"> = ['prim', "nil"]
const maintest2_div: Cion.RawLisp<"(/ 0 0)"> = ['prim', "nil"]
const maintest3_div: Cion.RawLisp<"(/ 4 4)"> = ['prim', ["0000000000000100", '0000000000000100']]
const maintest4_div: Cion.RawLisp<"(/ 4 2)"> = ['prim', ["0000000000000100", '0000000000000010']]
const maintest5_div: Cion.RawLisp<"(/ 4 (- 0 2))"> = ['prim', ['0000000000000100', "1111111111111110"]]
const maintest6_div: Cion.RawLisp<"(/ (- 2 6) (- 0 2))"> = ['prim', ["1111111111111100", "1111111111111110"]]
const maintest7_div: Cion.RawLisp<"(/ (- 2 6) -2)"> = ['prim', ["1111111111111100", "1111111111111110"]]

const maintest8_div: Cion.RawLisp<"(/ 2/3 -2)"> = ['prim', ["0000000000000010", "1111111111111010"]]
const maintest9_div: Cion.RawLisp<"(/ -2/3 -2)"> = ['prim', ["1111111111111110", "1111111111111010"]]
const maintest10_div: Cion.RawLisp<"(/ 1/9 (/ 2/3 3))"> = ['prim', ["0000000000001001", "0000000000010010"]]

const div_test0 : Cion.Lisp<`(/ 10 5)`> = '2'
const div_test1 : Cion.Lisp<`(/ 10 10)`> = '1'
const div_test2 : Cion.Lisp<`(/ 1 1)`> = '1'
const div_test3 : Cion.Lisp<`(/ 1 0)`> = 'nil'
const div_test4 : Cion.Lisp<`(/ 0 1)`> = '0'
const div_test5 : Cion.Lisp<`(/ 0 0)`> = 'nil'
const div_test6 : Cion.Lisp<`(/ 5 10)`> = '1/2'

const div_test0a : Cion.Lisp<`(/ 10/3 5/3)`> = '2'
const div_test1a : Cion.Lisp<`(/ 10/3 10/3)`> = '1'
const div_test2a : Cion.Lisp<`(/ 1/3 1/3)`> = '1'
const div_test3a : Cion.Lisp<`(/ 1/3 0)`> = 'nil'
const div_test6a : Cion.Lisp<`(/ 0 1/3)`> = '0'
const div_test4a : Cion.Lisp<`(/ 0 0)`> = 'nil'
const div_test5a : Cion.Lisp<`(/ 5/3 10/3)`> = '1/2'

const div_test0b : Cion.Lisp<`(/ 10/3 5/2)`> = '4/3'
const div_test1b : Cion.Lisp<`(/ 10/3 10/7)`> = '7/3'
const div_test2b : Cion.Lisp<`(/ 1/3 1/2)`> = '2/3'
const div_test3b : Cion.Lisp<`(/ 1/2 1/3)`> = '3/2'
const div_test4b : Cion.Lisp<`(/ 5/2 10/3)`> = '3/4'

const div_test0c : Cion.Lisp<`(/ 10/3 7)`> = '10/21'
const div_test1c : Cion.Lisp<`(/ 1/3 7)`> = '1/21'
const div_test2c : Cion.Lisp<`(/ 7 10/3)`> = '21/10'

const div_test_f0 : Cion.Lisp<`(/ 1 0)`> = 'nil'
const div_test_f1 : Cion.Lisp<`(/ 1 nil)`>['ast']['error'] = 'LispDivError2'
const div_est_f2 : Cion.Lisp<`(/ 1 true)`>['ast']['error'] = 'LispDivError2'
const div_test_f3 : Cion.Lisp<`(/ 1 'string')`>['ast']['error'] = 'LispDivError2'
const div_test_f4 : Cion.Lisp<`(/ 1 div)`>['ast']['error'] = 'LispDivError2'
const div_test_f5 : Cion.Lisp<`(/ 1 (fn [x] x))`>['ast']['error'] = 'LispDivError2'
const div_test_f6 : Cion.Lisp<`(/ 1 [])`>['ast']['error'] = 'LispDivError2'
const div_test_f7 : Cion.Lisp<`(/ 1 {})`>['ast']['error'] = 'LispDivError2'

const div_test_f8 : Cion.Lisp<`(/ 1 2/3)`> = '3/2'
const div_test_f : Cion.Lisp<`(/ 1 -2/3)`> = '-3/2'
