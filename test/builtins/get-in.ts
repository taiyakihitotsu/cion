import type Cion from '../../src/index'
import type {Equal} from '../../src/util'

const getIn_test_0 : true = {} as Equal<'[2]'
,Cion.Lisp<`(get-in [[2]] [0])`>>

const getIn_test_1 : true = {} as Equal<'2'
,Cion.Lisp<`(get-in [[2]] [0 0])`>>

const getIn_test_2 : true = {} as Equal<'2'
,Cion.Lisp<`(get-in [0 [2]] [1 0])`>>
const getIn_test_3 : true = {} as Equal<'2'
,Cion.Lisp<`(get-in {:a {:b 2}} [:a :b])`>>
const getIn_test_4 : true = {} as Equal<'{:b 2}'
,Cion.Lisp<`(get-in {:a {:b 2}} [:a])`>>
const getIn_test_5 : true = {} as Equal<'nil'
,Cion.Lisp<`(get-in {:a {:b 2}} [:c])`>>
const getIn_test_6 : true = {} as Equal<'nil'
,Cion.Lisp<`(get-in {:a {:b 2}} [:a :c])`>>

const getIn_test_7 : true = {} as Equal<'nil'
,Cion.Lisp<`(get-in {:a {:b 2}} [9 :c])`>>
const getIn_test_8 : true = {} as Equal<'nil'
,Cion.Lisp<`(get-in {:a {:b 2}} [:a 9])`>>
const getIn_test_9 : true = {} as Equal<'2'
,Cion.Lisp<`(get-in {:a [2]} [:a 0])`>>
const getIn_test_10 : true = {} as Equal<'2'
,Cion.Lisp<`(get-in [9 {:a [2]}] [1 :a 0])`>>
const getIn_test_11 : true = {} as Equal<'nil'
,Cion.Lisp<"(get-in [[0 1] 2] [9])">>
