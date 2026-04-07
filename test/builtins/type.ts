import type Cion from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// --- type (Type Detection) Tests ---

// Primitives
const type_test_0 : true = {} as Equal<`'number'`,  Cion.Lisp<`(type 9)`>>
const type_test_1 : true = {} as Equal<`'string'`,  Cion.Lisp<`(type '9')`>>
const type_test_2 : true = {} as Equal<`'boolean'`, Cion.Lisp<`(type true)`>>

// Collections
const type_test_3 : true = {} as Equal<`'vector'`,  Cion.Lisp<`(type [])`>>
const type_test_4 : true = {} as Equal<`'vector'`,  Cion.Lisp<`(type [9 8 7])`>>
const type_test_5 : true = {} as Equal<`'map'`,     Cion.Lisp<`(type {a: 9})`>>
const type_test_6 : true = {} as Equal<`'map'`,     Cion.Lisp<`(type {})`>>

// Functions and Symbols
const type_test_7 : true = {} as Equal<`'fn'`,      Cion.Lisp<`(type number?)`>>      // Built-in fn
const type_test_8 : true = {} as Equal<`'symbol'`,  Cion.Lisp<`(type some->)`>>      // Macro symbol
const type_test_9 : true = {} as Equal<`'fn'`,      Cion.Lisp<`(type (fn [n] (inc n)))`>> // Lambda

// Others
const type_test_10 : true = {} as Equal<`'key'`,    Cion.Lisp<`(type :key)`>>
const type_test_11 : true = {} as Equal<`'nil'`,    Cion.Lisp<`(type nil)`>>
