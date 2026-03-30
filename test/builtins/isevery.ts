import type Cion from '../../src/index'
import type { LispIsEvery } from '../../src/index'
import type {Equal} from '../../src/util'

const isevery_test0: true = {} as Equal<LispIsEvery<[['fn', [['sym', 'x']], [['sym', 'number?'], ['sym', 'x']]], ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001']]]>, ['prim', true]>
const isevery_test1: true = {} as Equal<LispIsEvery<[['fn', [['sym', 'x']], [['sym', 'number?'], ['sym', 'x']]], ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', `'0000000000000001'`]]]>, ['prim', false]>
const isevery_test2: true = {} as Equal<LispIsEvery<[['fn', [['sym', 'x']], [['sym', 'number?'], ['sym', 'x']]], ['vec']]>, ['prim', false]>

const isevery_list_test0: true = {} as Equal<Cion.Lisp<`(every? (fn [x] (number? x)) [0 1 2])`>, 'true'>
const isevery_list_test1: true = {} as Equal<Cion.Lisp<`(every? number? [0 1 2])`>, 'true'>
const isevery_list_test2: true = {} as Equal<Cion.Lisp<`(every? number? [0 1 '2'])`>, 'false'>
