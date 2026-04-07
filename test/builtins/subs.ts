import type Cion from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// Normal range selection
const subs_test_0 : true = {} as Equal<`'23'`,  Cion.Lisp<`(subs '123456' 1 3)`>>
const subs_test_1 : true = {} as Equal<`'123'`, Cion.Lisp<`(subs '123456' 0 3)`>>
const subs_test_2 : true = {} as Equal<`'456'`, Cion.Lisp<`(subs '123456' 3 6)`>>

// Index clamping (Out of bounds)
const subs_test_3 : true = {} as Equal<`'456'`,    Cion.Lisp<`(subs '123456' 3 8)`>>
const subs_test_4 : true = {} as Equal<`'123456'`, Cion.Lisp<`(subs '123456' -3 6)`>>

// Empty selection
const subs_test_5 : true = {} as Equal<`''`, Cion.Lisp<`(subs '123456' 2 2)`>>
const subs_test_6 : true = {} as Equal<`''`, Cion.Lisp<`(subs '123456' 0 0)`>>
const subs_test_7 : true = {} as Equal<`''`, Cion.Lisp<`(subs '123456' 6 6)`>>

// Omitted end index (Substring from start to end of string)
const subs_test_8 : true = {} as Equal<`'123456'`, Cion.Lisp<`(subs '123456' 0)`>>
const subs_test_9 : true = {} as Equal<`''`,       Cion.Lisp<`(subs '123456' 6)`>>
const subs_test_10 : true = {} as Equal<`'456'`,   Cion.Lisp<`(subs '123456' 3)`>>
