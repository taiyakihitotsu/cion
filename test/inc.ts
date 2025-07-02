import type Cion from '../src/index'

const inc_test0 : Cion.Lisp<`(inc 0)`> = '1'
const inc_test1 : Cion.Lisp<`(inc nil)`>['ast']['error'] = 'LispIncError0'
const inc_test2 : Cion.Lisp<`(inc true)`>['ast']['error'] = 'LispIncError0'
const inc_test3 : Cion.Lisp<`(inc 'string')`>['ast']['error'] = 'LispIncError0'
const inc_test4 : Cion.Lisp<`(inc inc)`>['ast']['error'] = 'LispIncError0'
const inc_test5 : Cion.Lisp<`(inc (fn [x] x))`>['ast']['error'] = 'LispIncError0'
const inc_test6 : Cion.Lisp<`(inc [])`>['ast']['error'] = 'LispIncError0'
const inc_test7 : Cion.Lisp<`(inc {})`>['ast']['error'] = 'LispIncError0'

const inc_test8 : Cion.Lisp<`(inc 2/3)`> = '5/3'
const inc_test : Cion.Lisp<`(inc -2/3)`> = '1/3'
