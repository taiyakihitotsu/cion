import type Cion from '../src/index'

const keys_test0 : Cion.Lisp<`(keys {:a 1 :b 2})`> = `[:a :b]`
const keys_test1 : Cion.Lisp<`(keys {})`> = `[]`
const keys_test2 : Cion.Lisp<`(keys ['x' 'y'])`> = '[0 1]'
const keys_test3 : Cion.Lisp<`(keys [])`> = '[]'
const keys_test : Cion.Lisp<`(keys 1)`> = `nil`
