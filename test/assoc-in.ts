import type Cion from '../src/index'
import type { _AssocIn } from '../src/index'

const testassocin0: _AssocIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '1']], ['prim', 10]> = ['vec', ['key', ':a'], ['prim', 10], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]
const testassocin1: _AssocIn<['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]], ['vec', ['key', ':a']], ['prim', 10]> = ['map', [['key', ':a'], ['prim', 10], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]]
const testassocin2: _AssocIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '1'], ['prim', '0']], ['prim', 10]> = 'AccessFailed' // {error: 'AssocInError8', message: "Keys rests but its value is not vector nor map.", sexpr: ['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]]}
const testassocin3a: _AssocIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '11'], ['prim', '1']], ['prim', 10]> = ['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 10]], ['key', ':c'], ['prim', 2]]
const testassocin3b: _AssocIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '11'], ['prim', '10']], ['prim', 10]> = 'AccessFailed' // ['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]]
const testassocin4: _AssocIn<['vec', ['prim', 3], ['prim', '"d"'], ['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['map', [['key', ':ca'], ['prim', 2]]]]]], ['vec', ['prim', '10'], ['key', ':c'], ['key', ':ca']], ['prim', 10]> = ['vec', ['prim', 3], ['prim', '"d"'], ['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['map', [['key', ':ca'], ['prim', 10]]]]]]

const maintest0_assocIn_0: Cion.RawLisp<'(assoc-in [0 1 2] [0] 99)'> = ['vec', ['prim', '0000000001100011'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_assocIn_1: Cion.RawLisp<'(assoc-in [0 1 2] [99] 99)'> = ['prim', 'nil'] // ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_assocIn_2: Cion.RawLisp<'(assoc-in [0 1 2] [0 0] 99)'> = ['prim', 'nil'] // {error: "AssocInError8", message: "Keys rests but its value is not vector nor map.", sexpr: ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]}
const maintest0_assocIn_3: Cion.RawLisp<'(assoc-in [0 1 [2 3 4]] [2 0] 99)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000001100011'], ['prim', '0000000000000011'], ['prim', '0000000000000100']]]
const maintest0_assocIn_4: Cion.RawLisp<'(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 0 :a] 99)'> = ['prim', 'nil'] // = {error: 'AssocInError7', message: `The value of key (0000000000000000) is not vector nor map.`, sexpr: ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['map', [['key', ':a'], ['prim', '0000000000000100'],['key', ':b'], ['prim', '0000000000000101']]]]]}
const maintest0_assocIn_5: Cion.RawLisp<'(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 2 :a] 99)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['map', [['key', ':a'], ['prim', '0000000001100011'], ['key', ':b'], ['prim', '0000000000000101']]]]]
const maintest0_assocIn_6: Cion.RawLisp<'(assoc-in {:x [0 1 [2 3 {:a 4 :b 5}]] :y 0} [:x 2 2 :a] 99)'> = ['map', [['key', ':x'], ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['map', [['key', ':a'], ['prim', '0000000001100011'], ['key', ':b'], ['prim', '0000000000000101']]]]], ['key', ':y'], ['prim', '0000000000000000']]]

const associntest_0: Cion.Lisp<`(assoc-in [[0 1] 2] [0 0] 100)`> = '[[100 1] 2]'
const associntest_1: Cion.Lisp<`(assoc-in {:a 0 :b {:c 1}} [:b :c] 100)`> = '{:a 0 :b {:c 100}}'
const associntest_2: Cion.Lisp<`(assoc-in {:a 0 :b 1} [:b :c] 100)`> = 'nil'
const associntest_3: Cion.Lisp<`(assoc-in [0 1] [2] 100)`> = 'nil'
const associntest_4: Cion.Lisp<`(assoc-in {:a [0 [1 2]]} [:a 1 1] 200)`> = '{:a [0 [1 200]]}'
const associntest_5: Cion.Lisp<`(assoc-in [5 6 {:a [0 [1 2]]}] [2 :a 1 1] 200)`> = '[5 6 {:a [0 [1 200]]}]'
const associntest_6: Cion.Lisp<`(assoc-in {:a 0 :b [0]} [:b 1] 100)`> = 'nil'
const associntest_7: Cion.Lisp<`(assoc-in [0 {:a 1}] [1 :b] 100)`> = 'nil'
