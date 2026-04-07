import type Cion from '../../src/index.js'
import type {Equal} from '../../src/util.js'

const mod_test_0 : true = {} as Equal<'0',   Cion.Lisp<`(mod 10 5)`>>
const mod_test_1 : true = {} as Equal<'0',   Cion.Lisp<`(mod 10 10)`>>
const mod_test_2 : true = {} as Equal<'0',   Cion.Lisp<`(mod 1 1)`>>
const mod_test_3 : true = {} as Equal<'nil', Cion.Lisp<`(mod 1 0)`>>
const mod_test_4 : true = {} as Equal<'0',   Cion.Lisp<`(mod 0 1)`>>
const mod_test_5 : true = {} as Equal<'nil', Cion.Lisp<`(mod 0 0)`>>
const mod_test_6 : true = {} as Equal<'5',   Cion.Lisp<`(mod 5 10)`>>
const mod_test_7 : true = {} as Equal<'2',   Cion.Lisp<`(mod 2 5)`>>
const mod_test_8 : true = {} as Equal<'2',   Cion.Lisp<`(mod 17 5)`>>
const mod_test_9 : true = {} as Equal<'2',   Cion.Lisp<`(mod 17 3)`>>
const mod_test_10 : true = {} as Equal<'5',  Cion.Lisp<`(mod 17 6)`>>
const mod_test_11 : true = {} as Equal<'-1', Cion.Lisp<`(mod 17 -6)`>>

const mod_test_12 : true = {} as Equal<'2',  Cion.Lisp<`(mod -10 3)`>>
const mod_test_13 : true = {} as Equal<'-2', Cion.Lisp<`(mod 10 -3)`>>
const mod_test_14 : true = {} as Equal<'-1', Cion.Lisp<`(mod -10 -3)`>>
const mod_test_15 : true = {} as Equal<'1',  Cion.Lisp<`(mod 10 3)`>>

const mod_rational_test_0 : true = {} as Equal<'0',    Cion.Lisp<`(mod 10/3 5/3)`>>
const mod_rational_test_1 : true = {} as Equal<'0',    Cion.Lisp<`(mod 10/3 10/3)`>>
const mod_rational_test_2 : true = {} as Equal<'0',    Cion.Lisp<`(mod 1/3 1/3)`>>
const mod_rational_test_3 : true = {} as Equal<'nil',  Cion.Lisp<`(mod 1/3 0)`>>
const mod_rational_test_4 : true = {} as Equal<'0',    Cion.Lisp<`(mod 0 1/3)`>>
const mod_rational_test_5 : true = {} as Equal<'5/3',  Cion.Lisp<`(mod 5/3 10/3)`>>

// Mixed signs with rationals
const mod_rational_test_6 : true = {} as Equal<'5/6',   Cion.Lisp<`(mod 10/3 5/2)`>>
const mod_rational_test_7 : true = {} as Equal<'-5/3',  Cion.Lisp<`(mod 10/3 -5/2)`>>
const mod_rational_test_8 : true = {} as Equal<'5/3',   Cion.Lisp<`(mod -10/3 5/2)`>>
const mod_rational_test_9 : true = {} as Equal<'-5/6',  Cion.Lisp<`(mod -10/3 -5/2)`>>

// Precision checks (Denominator 21)
const mod_rational_test_10 : true = {} as Equal<'17/21',  Cion.Lisp<`(mod 11/3 10/7)`>>
const mod_rational_test_11 : true = {} as Equal<'-13/21', Cion.Lisp<`(mod 11/3 -10/7)`>>
const mod_rational_test_12 : true = {} as Equal<'13/21',  Cion.Lisp<`(mod -11/3 10/7)`>>
const mod_rational_test_13 : true = {} as Equal<'-17/21', Cion.Lisp<`(mod -11/3 -10/7)`>>

// Smaller fractions
const mod_rational_test_14 : true = {} as Equal<'1/3',  Cion.Lisp<`(mod 1/3 1/2)`>>
const mod_rational_test_15 : true = {} as Equal<'-1/6', Cion.Lisp<`(mod 1/3 -1/2)`>>
const mod_rational_test_16 : true = {} as Equal<'1/6',  Cion.Lisp<`(mod -1/3 1/2)`>>
const mod_rational_test_17 : true = {} as Equal<'-1/3', Cion.Lisp<`(mod -1/3 -1/2)`>>

// Rational vs Integer
const mod_mixed_test_0 : true = {} as Equal<'10/3', Cion.Lisp<`(mod 10/3 7)`>>
const mod_mixed_test_1 : true = {} as Equal<'1/3',  Cion.Lisp<`(mod 1/3 7)`>>
const mod_mixed_test_2 : true = {} as Equal<'1/3',  Cion.Lisp<`(mod 7 10/3)`>>

// Error Handling (Type constraints)
type GetError<T> = T extends { ast: { error: infer E } } ? E : never

const mod_error_test_0 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(mod 1 nil)`>>>
const mod_error_test_1 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(mod 1 true)`>>>
const mod_error_test_2 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(mod 1 'string')`>>>
const mod_error_test_3 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(mod 1 (fn [x] x))`>>>
const mod_error_test_4 : true = {} as Equal<'LispRemOrModError2', GetError<Cion.Lisp<`(mod 1 [])`>>>
