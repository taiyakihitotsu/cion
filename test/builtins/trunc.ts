import type Cion from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// --- trunc (Truncation toward zero) Tests ---

// Integer cases
const trunc_test_0 : true = {} as Equal<'0',  Cion.Lisp<`(trunc 0)`>>
const trunc_test_1 : true = {} as Equal<'2',  Cion.Lisp<`(trunc 2)`>>
const trunc_test_2 : true = {} as Equal<'-2', Cion.Lisp<`(trunc -2)`>>

// Rational cases (Positive)
const trunc_test_3 : true = {} as Equal<'1',  Cion.Lisp<`(trunc 3/2)`>> // 1.5 -> 1
const trunc_test_4 : true = {} as Equal<'2',  Cion.Lisp<`(trunc 7/3)`>> // 2.33... -> 2
const trunc_test_5 : true = {} as Equal<'0',  Cion.Lisp<`(trunc 1/2)`>> // 0.5 -> 0

// Rational cases (Negative)
const trunc_test_6 : true = {} as Equal<'-1', Cion.Lisp<`(trunc -3/2)`>> // -1.5 -> -1
const trunc_test_7 : true = {} as Equal<'-2', Cion.Lisp<`(trunc -7/3)`>> // -2.33... -> -2
const trunc_test_8 : true = {} as Equal<'0',  Cion.Lisp<`(trunc -1/2)`>> // -0.5 -> 0
