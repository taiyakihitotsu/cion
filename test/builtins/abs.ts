import type Cion from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const test0: Equal<'0', Cion.Lisp<`(abs 0)`>> = true
const test1: Equal<'0', Cion.Lisp<`(abs 0)`>> = true
const test2: Equal<'1', Cion.Lisp<`(abs 1)`>> = true
const test3: Equal<'1', Cion.Lisp<`(abs -1)`>> = true
const test4: Equal<'2/3', Cion.Lisp<`(abs 2/3)`>> = true
const test5: Equal<'2/3', Cion.Lisp<`(abs -2/3)`>> = true
const test6: Equal<'nil', Cion.Lisp<`(abs nil)`>> = true
const test7: Equal<'6/5', Cion.Lisp<`(abs (* -2 -3/5))`>> = true
const test6_str: Equal<'nil', Cion.Lisp<`(abs 'test')`>> = true
