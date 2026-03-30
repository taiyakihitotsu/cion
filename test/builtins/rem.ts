import type Cion from '../../src/index.ts'
import type { Equal } from '../../src/util'

const rem_test_0 : true = {} as Equal<'0',   Cion.Lisp<`(rem 10 5)`>>
const rem_test_1 : true = {} as Equal<'0',   Cion.Lisp<`(rem 10 10)`>>
const rem_test_2 : true = {} as Equal<'0',   Cion.Lisp<`(rem 1 1)`>>
const rem_test_3 : true = {} as Equal<'nil', Cion.Lisp<`(rem 1 0)`>>
const rem_test_4 : true = {} as Equal<'0',   Cion.Lisp<`(rem 0 1)`>>
const rem_test_5 : true = {} as Equal<'nil', Cion.Lisp<`(rem 0 0)`>>
const rem_test_6 : true = {} as Equal<'5',   Cion.Lisp<`(rem 5 10)`>>
const rem_test_7 : true = {} as Equal<'2',   Cion.Lisp<`(rem 2 5)`>>
const rem_test_8 : true = {} as Equal<'2',   Cion.Lisp<`(rem 17 5)`>>
const rem_test_9 : true = {} as Equal<'2',   Cion.Lisp<`(rem 17 3)`>>
const rem_test_10 : true = {} as Equal<'5',  Cion.Lisp<`(rem 17 6)`>>
const rem_test_11 : true = {} as Equal<'5',  Cion.Lisp<`(rem 17 -6)`>>

const rem_signed_test_0 : true = {} as Equal<'-1', Cion.Lisp<`(rem -10 3)`>>
const rem_signed_test_1 : true = {} as Equal<'1',  Cion.Lisp<`(rem 10 -3)`>>
const rem_signed_test_2 : true = {} as Equal<'-1', Cion.Lisp<`(rem -10 -3)`>>
const rem_signed_test_3 : true = {} as Equal<'1',  Cion.Lisp<`(rem 10 3)`>>

const rem_rational_test_0 : true = {} as Equal<'0',    Cion.Lisp<`(rem 10/3 5/3)`>>
const rem_rational_test_1 : true = {} as Equal<'0',    Cion.Lisp<`(rem 10/3 10/3)`>>
const rem_rational_test_2 : true = {} as Equal<'0',    Cion.Lisp<`(rem 1/3 1/3)`>>
const rem_rational_test_3 : true = {} as Equal<'nil',  Cion.Lisp<`(rem 1/3 0)`>>
const rem_rational_test_4 : true = {} as Equal<'0',    Cion.Lisp<`(rem 0 1/3)`>>
const rem_rational_test_5 : true = {} as Equal<'5/3',  Cion.Lisp<`(rem 5/3 10/3)`>>

// Rational mixed signs
const rem_rational_test_6 : true = {} as Equal<'5/6',   Cion.Lisp<`(rem 10/3 5/2)`>>
const rem_rational_test_7 : true = {} as Equal<'5/6',   Cion.Lisp<`(rem 10/3 -5/2)`>>
const rem_rational_test_8 : true = {} as Equal<'-5/6',  Cion.Lisp<`(rem -10/3 5/2)`>>
const rem_rational_test_9 : true = {} as Equal<'-5/6',  Cion.Lisp<`(rem -10/3 -5/2)`>>

// Precision checks
const rem_rational_test_10 : true = {} as Equal<'17/21',  Cion.Lisp<`(rem 11/3 10/7)`>>
const rem_rational_test_11 : true = {} as Equal<'17/21',  Cion.Lisp<`(rem 11/3 -10/7)`>>
const rem_rational_test_12 : true = {} as Equal<'-17/21', Cion.Lisp<`(rem -11/3 10/7)`>>
const rem_rational_test_13 : true = {} as Equal<'-17/21', Cion.Lisp<`(rem -11/3 -10/7)`>>

// Smaller fractions
const rem_rational_test_14 : true = {} as Equal<'1/3',  Cion.Lisp<`(rem 1/3 1/2)`>>
const rem_rational_test_15 : true = {} as Equal<'1/3',  Cion.Lisp<`(rem 1/3 -1/2)`>>
const rem_rational_test_16 : true = {} as Equal<'-1/3', Cion.Lisp<`(rem -1/3 1/2)`>>
const rem_rational_test_17 : true = {} as Equal<'-1/3', Cion.Lisp<`(rem -1/3 -1/2)`>>
const rem_rational_test_18 : true = {} as Equal<'1/6',  Cion.Lisp<`(rem 1/2 1/3)`>>
const rem_rational_test_19 : true = {} as Equal<'1/6',  Cion.Lisp<`(rem 1/2 -1/3)`>>
const rem_rational_test_20 : true = {} as Equal<'-1/6', Cion.Lisp<`(rem -1/2 1/3)`>>
const rem_rational_test_21 : true = {} as Equal<'-1/6', Cion.Lisp<`(rem -1/2 -1/3)`>>

// Rational vs Integer
const rem_mixed_test_0 : true = {} as Equal<'10/3', Cion.Lisp<`(rem 10/3 7)`>>
const rem_mixed_test_1 : true = {} as Equal<'1/3',  Cion.Lisp<`(rem 1/3 7)`>>
const rem_mixed_test_2 : true = {} as Equal<'1/3',  Cion.Lisp<`(rem 7 10/3)`>>

// Error Handling
type GetError<T> = T extends { ast: { error: infer E } } ? E : never

const rem_error_test_0 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(rem 1 nil)`>>>
const rem_error_test_1 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(rem 1 true)`>>>
const rem_error_test_2 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(rem 1 'string')`>>>
const rem_error_test_3 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(rem 1 rem)`>>>
const rem_error_test_4 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(rem 1 (fn [x] x))`>>>
const rem_error_test_5 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(rem 1 [])`>>>
const rem_error_test_6 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(rem 1 {})`>>>

// Final checks
const rem_final_test_0 : true = {} as Equal<'1/3', Cion.Lisp<`(rem 1 2/3)`>>
const rem_final_test_1 : true = {} as Equal<'1/3', Cion.Lisp<`(rem 1 -2/3)`>>
