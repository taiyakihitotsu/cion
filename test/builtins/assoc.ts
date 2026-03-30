import type Cion from '../../src/index'
import type { _Assoc } from '../../src/index' // [todo]
import type { Equal } from '../../src/util'

const assoc_test_0: true = {} as Equal<['vec', ['key', ':a'], ['prim', 10], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]], _Assoc<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]], ['prim', '1'], ['prim', 10]>>

const assoc_test_1 : true = {} as Equal<['vec', ['prim', 10], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]], _Assoc<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]], ['prim', '0'], ['prim', 10]>>

const assoc_test_2 : true = {} as Equal<['prim', 'nil'], _Assoc<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]], ['prim', '1111'], ['prim', 10]>>

const assoc_test_3 : true = {} as Equal<['map', [['key', ':a'], ['prim', 10], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]], _Assoc<['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]], ['key', ':a'], ['prim', 10]>>

const assoc_test_4 : true = {} as Equal<['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 10], ['key', ':c'], ['prim', 2]]], _Assoc<['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]], ['key', ':b'], ['prim', 10]>>

const assoc_test_5 : true = {} as Equal<['prim', 'nil'], _Assoc<['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]], ['key', ':d'], ['prim', 10]>>

const assoc_test_6 : true = {} as Equal<['vec', ['prim', '0000000001100011'], ['prim', '0000000000000001'], ['prim', '0000000000000010']], Cion.RawLisp<'(assoc [0 1 2] 0 99)'>>

const assoc_test_7 : true = {} as Equal<['prim', 'nil'], Cion.RawLisp<'(assoc [0 1 2] 3 99)'>>

const assoc_test_8 : true = {} as Equal<'[100 1]', Cion.Lisp<`(assoc [0 1] 0 100)`>>
const assoc_test_9 : true = {} as Equal<'{:a 100 :b 1}', Cion.Lisp<`(assoc {:a 0 :b 1} :a 100)`>>
const assoc_test_10 : true = {} as Equal<'nil', Cion.Lisp<`(assoc {:a 0 :b 1} :c 100)`>>
const assoc_test_11 : true = {} as Equal<'nil', Cion.Lisp<`(assoc [0 1] 2 100)`>>
const assoc_test_12 : true = {} as Equal<'nil', Cion.Lisp<`(assoc [] 0 100)`>>
const assoc_test_13 : true = {} as Equal<'nil', Cion.Lisp<`(assoc {} :a 100)`>>
