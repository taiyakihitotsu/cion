import type Cion from '../src/index'

const some_list_test0: Cion.Lisp<`(some (fn [x] (number? x)) [0 1 2])`> = 'true'
const some_list_test1: Cion.Lisp<`(some number? [0 1 2])`> = 'true'
const some_list_test2: Cion.Lisp<`(some (fn [x] (number? x)) [0 '1' 2])`> = 'true'
const some_list_test3: Cion.Lisp<`(some number? [])`> = 'false'
const some_list_test4: Cion.Lisp<`(some number? ['1' '1'])`> = 'false'

