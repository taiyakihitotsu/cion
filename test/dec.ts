import type Cion from '../src/index'

const dec_test0 : Cion.Lisp<`(dec 0)`> = '-1'
const dec_test1 : Cion.Lisp<`(dec nil)`>['ast']['error'] = 'LispDecError0'
const dec_test2 : Cion.Lisp<`(dec true)`>['ast']['error'] = 'LispDecError0'
const dec_test3 : Cion.Lisp<`(dec 'string')`>['ast']['error'] = 'LispDecError0'
const dec_test4 : Cion.Lisp<`(dec dec)`>['ast']['error'] = 'LispDecError0'
const dec_test5 : Cion.Lisp<`(dec (fn [x] x))`>['ast']['error'] = 'LispDecError0'
const dec_test6 : Cion.Lisp<`(dec [])`>['ast']['error'] = 'LispDecError0'
const dec_test7 : Cion.Lisp<`(dec {})`>['ast']['error'] = 'LispDecError0'

const dec_test8 : Cion.Lisp<`(dec 2/3)`> = '-1/3'
const dec_test : Cion.Lisp<`(dec -2/3)`> = '-5/3'

const dec_test9: Cion.Lisp<`(dec -1)`> = '-2'
