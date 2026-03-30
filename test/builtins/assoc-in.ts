import type Cion from '../../src/index'
import type { _AssocIn } from '../../src/index'
import type { Equal } from '../../src/util'

const assocIn_test_0: true = {} as Equal<['vec', ['key', ':a'], ['prim', 10], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]],  _AssocIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '1']], ['prim', 10]>>
const assocIn_test_1: true = {} as Equal<['map', [['key', ':a'], ['prim', 10], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]], _AssocIn<['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]], ['vec', ['key', ':a']], ['prim', 10]>>
const assocIn_test_2: true = {} as Equal<'AccessFailed', _AssocIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '1'], ['prim', '0']], ['prim', 10]>>
const assocIn_test_3: true = {} as Equal<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 10]], ['key', ':c'], ['prim', 2]], _AssocIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '11'], ['prim', '1']], ['prim', 10]>>
const assocIn_test_4: true = {} as Equal<'AccessFailed', _AssocIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '11'], ['prim', '10']], ['prim', 10]>>
const assocIn_test_5: true = {} as Equal<['vec', ['prim', 3], ['prim', '"d"'], ['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['map', [['key', ':ca'], ['prim', 10]]]]]], _AssocIn<['vec', ['prim', 3], ['prim', '"d"'], ['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['map', [['key', ':ca'], ['prim', 2]]]]]], ['vec', ['prim', '10'], ['key', ':c'], ['key', ':ca']], ['prim', 10]>>
const assocIn_test_6: true = {} as Equal<['vec', ['prim', '0000000001100011'], ['prim', '0000000000000001'], ['prim', '0000000000000010']], Cion.RawLisp<'(assoc-in [0 1 2] [0] 99)'>>
const assocIn_test_7: true = {} as Equal<['prim', 'nil'], Cion.RawLisp<'(assoc-in [0 1 2] [99] 99)'>>
const assocIn_test_8: true = {} as Equal<['prim', 'nil'], Cion.RawLisp<'(assoc-in [0 1 2] [0 0] 99)'>>
const assocIn_test_9: true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000001100011'], ['prim', '0000000000000011'], ['prim', '0000000000000100']]], Cion.RawLisp<'(assoc-in [0 1 [2 3 4]] [2 0] 99)'>>

const assocIn_keyAndRecord_test_0: true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['map', [['key', ':a'], ['prim', '0000000001100011'], ['key', ':b'], ['prim', '0000000000000101']]]]], Cion.RawLisp<'(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 2 :a] 99)'>>
const assocIn_keyAndNotRecord_test_0: true = {} as Equal<['prim', 'nil'], Cion.RawLisp<'(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 0 :a] 99)'>>
const assocIn_keyAndRecord_test_1: true = {} as Equal<['map', [['key', ':x'], ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['map', [['key', ':a'], ['prim', '0000000001100011'], ['key', ':b'], ['prim', '0000000000000101']]]]], ['key', ':y'], ['prim', '0000000000000000']]], Cion.RawLisp<'(assoc-in {:x [0 1 [2 3 {:a 4 :b 5}]] :y 0} [:x 2 2 :a] 99)'>>

// String Literal Test
const assocIn_sexpr_test_0: true = {} as Equal<'[[100 1] 2]', Cion.Lisp<`(assoc-in [[0 1] 2] [0 0] 100)`>>
const assocIn_sexpr_test_1: true = {} as Equal<'{:a 0 :b {:c 100}}', Cion.Lisp<`(assoc-in {:a 0 :b {:c 1}} [:b :c] 100)`>>
const assocIn_sexpr_test_2: true = {} as Equal<'nil', Cion.Lisp<`(assoc-in {:a 0 :b 1} [:b :c] 100)`>>
const assocIn_sexpr_test_3: true = {} as Equal<'nil', Cion.Lisp<`(assoc-in [0 1] [2] 100)`>>
const assocIn_sexpr_test_4: true = {} as Equal<'{:a [0 [1 200]]}', Cion.Lisp<`(assoc-in {:a [0 [1 2]]} [:a 1 1] 200)`>>
const assocIn_sexpr_test_5: true = {} as Equal<'[5 6 {:a [0 [1 200]]}]', Cion.Lisp<`(assoc-in [5 6 {:a [0 [1 2]]}] [2 :a 1 1] 200)`>>
const assocIn_sexpr_test_6: true = {} as Equal<'nil', Cion.Lisp<`(assoc-in {:a 0 :b [0]} [:b 1] 100)`>>
const assocIn_sexpr_test_7: true = {} as Equal<'nil', Cion.Lisp<`(assoc-in [0 {:a 1}] [1 :b] 100)`>>
