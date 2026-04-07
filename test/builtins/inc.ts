import type Cion from '../../src/index.js'
import type {Equal} from '../../src/util.js'

const inc_test_0 : true = {} as Equal<'1'
  ,Cion.Lisp<`(inc 0)`>>

const inc_test_1 : true = {} as Equal<'LispIncError0'
  ,Cion.Lisp<`(inc nil)`>['ast']['error']>

const inc_test_2 : true = {} as Equal<'LispIncError0'
  ,Cion.Lisp<`(inc true)`>['ast']['error']>
const inc_test_3 : true = {} as Equal<'LispIncError0'
  ,Cion.Lisp<`(inc 'string')`>['ast']['error']>

const inc_test_4 : true = {} as Equal<'LispIncError0'
  ,Cion.Lisp<`(inc inc)`>['ast']['error']>

const inc_test_5 : true = {} as Equal<'LispIncError0'
  ,Cion.Lisp<`(inc (fn [x] x))`>['ast']['error']>

const inc_test_6 : true = {} as Equal<'LispIncError0'
  ,Cion.Lisp<`(inc [])`>['ast']['error']>

const inc_test_7 : true = {} as Equal<'LispIncError0'
  ,Cion.Lisp<`(inc {})`>['ast']['error']>

const inc_test_8 : true = {} as Equal<'5/3'
  ,Cion.Lisp<`(inc 2/3)`>>

const inc_test_9 : true = {} as Equal<'1/3'
  ,Cion.Lisp<`(inc -2/3)`>>
