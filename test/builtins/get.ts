import type Cion from '../../src/index.js'
import type { Get, LispGet, Eval, IsKeyMapSexpr } from '../../src/index.js'
import {VNil} from '../../src/sexprtypes.js'
import type {Equal} from '../../src/util.js'

const get_test_0 : true = {} as Equal<false
  ,IsKeyMapSexpr<[['key', ':a'], ['key', ':b']]>>

const get_test_1 : true = {} as Equal<false
  ,IsKeyMapSexpr<[['map', [['key', ':a'], ['prim', '0']]], ['map', [['key', ':a'], ['prim', '1']]]]>>

const get_test_2 : true = {} as Equal<true
  ,IsKeyMapSexpr<[['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]]>>

const get_test_3 : true = {} as Equal<true
  ,IsKeyMapSexpr<[['map', [['key', ':a'], ['prim', '0']]], ['key', ':a']]>>

const get_test_4 : true = {} as Equal<true
  ,IsKeyMapSexpr<[['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]]>>

const get_test_5 : true = {} as Equal<['prim', '0']
  ,Eval<[['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]]>>

const get_test_6 : true = {} as Equal<true
  ,IsKeyMapSexpr<[['map', [['key', ':a'], ['prim', '0']]], ['key', ':a']]>>

const get_test_7 : true = {} as Equal<['prim', '0']
  ,Eval<[['map', [['key', ':a'], ['prim', '0']]], ['key', ':a']]>>

const get_test_8 : true = {} as Equal<['prim', '0']
  ,LispGet<[['map', [['key', ':a'], ['prim', '0']]], ['key', ':a']]>>

const get_test_9 : true = {} as Equal<typeof VNil
  ,LispGet<[['map', [['key', ':'], ['prim', '0']]], ['key', ':a']]>>


type Actual_get_11 = Get<['prim', '11'], [`vec`, [`prim`, 0], [`prim`, 1], [`prim`, 2], [`prim`, 3], [`prim`, 4]]>
const get_test_10 : true = {} as Equal<[`prim`, 3], Actual_get_11>

const get_test_11 : true = {} as Equal<['prim', '0']
  ,Get<['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]>>

const get_test_12 : true = {} as Equal<['prim', '0']
  ,Get<['key', ':a'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]>>

const get_test_13 : true = {} as Equal<typeof VNil
  ,Get<['key', ':c'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]>>

const get_test_14 : true = {} as Equal<['key', ':a']
  ,Get<['prim', '0'], ['vec', ['key', ':a'], ['prim', '0']]>>

type Actual_get_15 = Get<['prim', '1'], ['vec', ['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]>
const get_test_15 : true = {} as Equal<['prim', '10'], Actual_get_15>

const get_test_16 : true = {} as Equal<typeof VNil
  ,Get<['prim', '111'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]>>

const get_test_17 : true = {} as Equal<'0'
  ,Cion.Lisp<`(get [0 1] 0)`>>

const get_test_18 : true = {} as Equal<'nil'
  ,Cion.Lisp<`(get [0 1] 2)`>>

// @ts-expect-error:
const maintest_get2: Cion.Lisp<`(get [0 1] -1)`> = {ast: [], error: 'InnerUnparseError'}

// @ts-expect-error:
const maintest_get5: Cion.Lisp<`(get {:a 1 :b 2} -1)`> = {ast: [], error: 'InnerUnparseError'}
const get_test_19 : true = {} as Equal<'1'
  ,Cion.Lisp<`(get {:a 1 :b 2} :a)`>>

const get_test_20 : true = {} as Equal<'nil'
  ,Cion.Lisp<`(get {:a 1 :b 2} :c)`>>

const get_test_21 : true = {} as Equal<'1'
  ,Cion.Lisp<`(:a {:a 1 :b 2})`>>

const get_test_22 : true = {} as Equal<'2'
  ,Cion.Lisp<`(:b {:a 1 :b 2})`>>

const get_test_23 : true = {} as Equal<'nil'
  ,Cion.Lisp<`(:c {:a 1 :b 2})`>>

const get_test_24 : true = {} as Equal<'0'
  ,Cion.Lisp<`(get [0 1] (first [0 1]))`>>

const get_test_25 : true = {} as Equal<'1'
  ,Cion.Lisp<`(get {:a 1 :b 2} (first [:a :b]))`>>

const get_test_26 : true = {} as Equal<'0'
  ,Cion.Lisp<`(get [0 1] ((fn [n] (n [0 1])) first))`>>

const get_test_27 : true = {} as Equal<'0'
  ,Cion.Lisp<`((fn [n] (n [0 1])) first)`>>

const get_test_28 : true = {} as Equal<'0'
  ,Cion.Lisp<`(let [n first] (n [0 1]))`>>

const get_test_29 : true = {} as Equal<'0'
  ,Cion.Lisp<`(first [0 1])`>>

const get_test_30 : true = {} as Equal<'1'
  ,Cion.Lisp<`(get {:a 1 :b 2} ((fn [n] (n [:a :b])) first))`>>

const get_test_31 : true = {} as Equal<'0'
  ,Cion.Lisp<`(get ((fn [] [0 1])) (first [0 1]))`>>

const get_test_32 : true = {} as Equal<'1'
  ,Cion.Lisp<`(get ((fn [] {:a 1 :b 2})) (first [:a :b]))`>>

const get_test_33 : true = {} as Equal<'1'
  ,Cion.Lisp<`(get ((fn [n] (n [{:a 1 :b 2}])) first) :a)`>>
