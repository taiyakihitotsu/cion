import type Cion from '../../src/index'
import type { Equal } from '../../src/util'

// Normal range
const subs_all_test_0 : true = {} as Equal<`['1' '23' '456']`, Cion.Lisp<`(subs-all '123456' 1 3)`>>
const subs_all_test_1 : true = {} as Equal<`'1'`,            Cion.Lisp<`(first (subs-all '123456' 1 3))`>>

// Start from 0
const subs_all_test_2 : true = {} as Equal<`['' '123' '456']`, Cion.Lisp<`(subs-all '123456' 0 3)`>>
const subs_all_test_3 : true = {} as Equal<`''`,             Cion.Lisp<`(first (subs-all '123456' 0 3))`>>

// End at length (or beyond)
const subs_all_test_4 : true = {} as Equal<`['123' '456' '']`, Cion.Lisp<`(subs-all '123456' 3 6)`>>
const subs_all_test_5 : true = {} as Equal<`['123' '456' '']`, Cion.Lisp<`(subs-all '123456' 3 8)`>>

// Negative start (clamped to 0)
const subs_all_test_6 : true = {} as Equal<`['' '123456' '']`, Cion.Lisp<`(subs-all '123456' -3 6)`>>

// Empty selection (start == end)
const subs_all_test_7 : true = {} as Equal<`['12' '' '3456']`, Cion.Lisp<`(subs-all '123456' 2 2)`>>
const subs_all_test_8 : true = {} as Equal<`['' '' '123456']`, Cion.Lisp<`(subs-all '123456' 0 0)`>>
const subs_all_test_9 : true = {} as Equal<`['123456' '' '']`, Cion.Lisp<`(subs-all '123456' 6 6)`>>
