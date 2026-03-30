import type Cion from '../../src/index'
import type { Equal } from '../../src/util'

const dec_test_0 : true = {} as Equal<'-1', Cion.Lisp<`(dec 0)`>>
const dec_test_1 : true = {} as Equal<'LispDecError0', Cion.Lisp<`(dec nil)`>['ast']['error']>
const dec_test_2 : true = {} as Equal<'LispDecError0', Cion.Lisp<`(dec true)`>['ast']['error']>
const dec_test_3 : true = {} as Equal<'LispDecError0', Cion.Lisp<`(dec 'string')`>['ast']['error']>
const dec_test_4 : true = {} as Equal<'LispDecError0', Cion.Lisp<`(dec dec)`>['ast']['error']>
const dec_test_5 : true = {} as Equal<'LispDecError0', Cion.Lisp<`(dec (fn [x] x))`>['ast']['error']>
const dec_test_50 : true = {} as Equal<'LispDecError0', Cion.Lisp<`(dec [])`>['ast']['error']>
const dec_test_6 : true = {} as Equal<'LispDecError0', Cion.Lisp<`(dec {})`>['ast']['error']>
const dec_test_7 : true = {} as Equal<'-1/3', Cion.Lisp<`(dec 2/3)`>>
const dec_test_8: true = {} as Equal<'-5/3', Cion.Lisp<`(dec -2/3)`>>
const dec_test_9 : true = {} as Equal<'-2', Cion.Lisp<`(dec -1)`>>
