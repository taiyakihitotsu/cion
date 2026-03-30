import type Cion from '../../src/index'
import type { Equal } from '../../src/util'

// Basic split by literal string
const split_test_0 : true = {} as Equal<"['a' 'a' 'a' 'a' 'a']", Cion.Lisp<`(split 'ababababa' 'b')`>>

// Split by regex character class
const split_test_1 : true = {} as Equal<"['ad' 'df']", Cion.Lisp<`(split 'ad89df89' '[89]')`>>

// Split by regex with quantifier (+)
const split_test_2 : true = {} as Equal<"['ad' 'df']", Cion.Lisp<`(split 'ad89df89' '[89]+')`>>

// Split with trailing characters
const split_test_3 : true = {} as Equal<"['ad' 'df' 'zz']", Cion.Lisp<`(split 'ad89df89zz' '[89]+')`>>
