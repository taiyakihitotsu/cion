import type Cion from '../../src/index.js'
import type { LispDiv } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const div_test_0 : true = {} as Equal<[`prim`, ['0000000000000011', '0000000000000001']],
  LispDiv<[[`prim`, '00000011'], [`prim`, '0000001']]>>
const div_test_1 : true = {} as Equal<[`prim`, ['0000000000001111', '0000000000000011']],
  LispDiv<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]>>
const div_test_2 : true = {} as Equal<[`prim`, 'nil'],
  LispDiv<[[`prim`, '00000011'], [`prim`, '0000000']]>>
const div_test_3 : true = {} as Equal<['prim', ["0000000000000100", '0000000000000001']],
  Cion.RawLisp<"(/ 4 1)">>
const div_test_4 : true = {} as Equal<['prim', "nil"],
  Cion.RawLisp<"(/ 1 0)">>
const div_test_5 : true = {} as Equal<['prim', "nil"],
  Cion.RawLisp<"(/ 0 0)">>
const div_test_6 : true = {} as Equal<['prim', ["0000000000000100", '0000000000000100']],
  Cion.RawLisp<"(/ 4 4)">>
const div_test_7 : true = {} as Equal<['prim', ["0000000000000100", '0000000000000010']],
  Cion.RawLisp<"(/ 4 2)">>
const div_test_8 : true = {} as Equal<['prim', ['0000000000000100', "1111111111111110"]],
  Cion.RawLisp<"(/ 4 (- 0 2))">>
const div_test_9 : true = {} as Equal<['prim', ["1111111111111100", "1111111111111110"]],
  Cion.RawLisp<"(/ (- 2 6) (- 0 2))">>
const div_test_10 : true = {} as Equal<['prim', ["1111111111111100", "1111111111111110"]],
  Cion.RawLisp<"(/ (- 2 6) -2)">>
const div_test_11 : true = {} as Equal<['prim', ["0000000000000010", "1111111111111010"]],
  Cion.RawLisp<"(/ 2/3 -2)">>
const div_test_12 : true = {} as Equal<['prim', ["1111111111111110", "1111111111111010"]],
  Cion.RawLisp<"(/ -2/3 -2)">>
const div_test_13 : true = {} as Equal<['prim', ["0000000000001001", "0000000000010010"]],
  Cion.RawLisp<"(/ 1/9 (/ 2/3 3))">>

// String Expression Tests
const div_test_14 : true = {} as Equal<'2', Cion.Lisp<`(/ 10 5)`>>
const div_test_15 : true = {} as Equal<'1', Cion.Lisp<`(/ 10 10)`>>
const div_test_16 : true = {} as Equal<'1', Cion.Lisp<`(/ 1 1)`>>
const div_test_17 : true = {} as Equal<'nil', Cion.Lisp<`(/ 1 0)`>>
const div_test_18 : true = {} as Equal<'0', Cion.Lisp<`(/ 0 1)`>>
const div_test_19 : true = {} as Equal<'nil', Cion.Lisp<`(/ 0 0)`>>
const div_test_20 : true = {} as Equal<'1/2', Cion.Lisp<`(/ 5 10)`>>

const div_test_21 : true = {} as Equal<'2', Cion.Lisp<`(/ 10/3 5/3)`>>
const div_test_22 : true = {} as Equal<'1', Cion.Lisp<`(/ 10/3 10/3)`>>
const div_test_23 : true = {} as Equal<'1', Cion.Lisp<`(/ 1/3 1/3)`>>
const div_test_24 : true = {} as Equal<'nil', Cion.Lisp<`(/ 1/3 0)`>>
const div_test_25 : true = {} as Equal<'0', Cion.Lisp<`(/ 0 1/3)`>>
const div_test_26 : true = {} as Equal<'nil', Cion.Lisp<`(/ 0 0)`>>
const div_test_27 : true = {} as Equal<'1/2', Cion.Lisp<`(/ 5/3 10/3)`>>

const div_test_28 : true = {} as Equal<'4/3', Cion.Lisp<`(/ 10/3 5/2)`>>
const div_test_29 : true = {} as Equal<'7/3', Cion.Lisp<`(/ 10/3 10/7)`>>
const div_test_30 : true = {} as Equal<'2/3', Cion.Lisp<`(/ 1/3 1/2)`>>
const div_test_31 : true = {} as Equal<'3/2', Cion.Lisp<`(/ 1/2 1/3)`>>

const div_test_32 : true = {} as Equal<'3/4', Cion.Lisp<`(/ 5/2 10/3)`>>
const div_test_33 : true = {} as Equal<'10/21', Cion.Lisp<`(/ 10/3 7)`>>
const div_test_34 : true = {} as Equal<'1/21', Cion.Lisp<`(/ 1/3 7)`>>
const div_test_35 : true = {} as Equal<'21/10', Cion.Lisp<`(/ 7 10/3)`>>
const div_test_36 : true = {} as Equal<'nil', Cion.Lisp<`(/ 1 0)`>>
const div_test_37 : true = {} as Equal<'LispDivError2', Cion.Lisp<`(/ 1 nil)`>['ast']['error']>
const div_test_38 : true = {} as Equal<'LispDivError2', Cion.Lisp<`(/ 1 true)`>['ast']['error']>
const div_test_39 : true = {} as Equal<'LispDivError2', Cion.Lisp<`(/ 1 'string')`>['ast']['error']>
const div_test_40 : true = {} as Equal<'LispDivError2', Cion.Lisp<`(/ 1 div)`>['ast']['error']>
const div_test_41 : true = {} as Equal<'LispDivError2', Cion.Lisp<`(/ 1 (fn [x] x))`>['ast']['error']>
const div_test_42 : true = {} as Equal<'LispDivError2', Cion.Lisp<`(/ 1 [])`>['ast']['error']>
const div_test_43 : true = {} as Equal<'LispDivError2', Cion.Lisp<`(/ 1 {})`>['ast']['error']>
const div_test_44 : true = {} as Equal<'3/2', Cion.Lisp<`(/ 1 2/3)`>>
const div_test_45 : true = {} as Equal<'-3/2', Cion.Lisp<`(/ 1 -2/3)`>>
