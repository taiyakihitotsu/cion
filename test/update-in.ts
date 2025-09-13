import type Cion from '../src/index'
import type { _UpdateIn } from '../src/index'

const testupdatein0: _UpdateIn<['vec', ['prim', '0'], ['prim', '1'], ['prim', '10'], ['prim', '11'], ['prim', '100'], ['prim', '101']], ['vec', ['prim', '1']], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]> = ['vec', ['prim', '0'], ['prim', ['0000000000001001', '0000000000000001']], ['prim', '10'], ['prim', '11'], ['prim', '100'], ['prim', '101']]
const testupdatein1: _UpdateIn<['map', [['key', ':a'], ['prim', '1'], ['key', ':b'], ['prim', '10'], ['key', ':c'], ['prim', '11']]], ['vec', ['key', ':a']], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]> = ['map', [['key', ':a'], ['prim', ['0000000000001001', '0000000000000001']], ['key', ':b'], ['prim', '10'], ['key', ':c'], ['prim', '11']]]
// @ts-expect-error:
const testupdatein2: _UpdateIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]], ['vec', ['key', ':notfound']], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]> = ['prim', 'nil']
const testupdatein3a: _UpdateIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', '0001']], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '11'], ['prim', '01']], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]> = ['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', ['0000000000001001', '0000000000000001']]], ['key', ':c'], ['prim', 2]]
const testupdatein3b: _UpdateIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '11'], ['prim', '10']], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]> = 'AccessFailed' // ['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]] // note : no effect because of a value of the key doesn't exist (in current).
const testupdatein4: _UpdateIn<['vec', ['prim', '11'], ['prim', '"d"'], ['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1'], ['key', ':c'], ['map', [['key', ':ca'], ['prim', '10']]]]]], ['vec', ['prim', '10'], ['key', ':c'], ['key', ':ca']], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]> = ['vec', ['prim', '11'], ['prim', '"d"'], ['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1'], ['key', ':c'], ['map', [['key', ':ca'], ['prim', ['0000000000001010', '0000000000000001']]]]]]]

const maintest0_updateIn_0: Cion.RawLisp<'(update-in [0 1 2] [1] (fn [x] (+ x 99)))'> = ['vec', ['prim', '0000000000000000'], ['prim', ['0000000001100100', '0000000000000001']], ['prim', '0000000000000010']]
const maintest0_updateIn_1: Cion.RawLisp<'(update-in [0 1 2] [99] (fn [x] (+ x 99)))'> = ['prim', 'nil'] // ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]

const maintest0_updateIn_2: Cion.RawLisp<'(update-in [0 1 2] [99 99] (fn [x] (+ x 99)))'> = ['prim', 'nil']

const lisptestupdatein1:   Cion.Lisp<`(update-in [[2]] [0] first)`> = "[2]"
const lisptestupdatein1f:  Cion.Lisp<`(update-in [[2]] [0] (fn [n] (first n)))`> = '[2]'
const lisptestupdatein2:   Cion.Lisp<`(update-in [[2]] [0 0] inc)`> = '[[3]]'
const lisptestupdatein2f:  Cion.Lisp<`(update-in [[2]] [0 0] (fn [n] (inc n)))`> = '[[3]]'
const lisptestupdatein3:   Cion.Lisp<`(update-in [0 [2]] [1 0] inc)`> = '[0 [3]]'
const lisptestupdatein4n:  Cion.Lisp<`(update-in [0 [2]] [1 0 2] inc)`> = 'nil'
const lisptestupdatein4nb: Cion.Lisp<`(update-in [0 {:a 2}] [1 :a 2] inc)`> = 'nil'
const lisptestupdatein4nc: Cion.Lisp<`(update-in {:a [2]} [1 0 2] inc)`> = 'nil'
const lisptestupdatein4nd: Cion.Lisp<`(update-in {:a [2]} [:a 2 9] inc)`> = 'nil'
const lisptestupdatein1a:  Cion.Lisp<`(update-in {:a {:b 2}} [:a :b] inc)`> = '{:a {:b 3}}'
const lisptestpupdatein2a: Cion.Lisp<`(update-in {:a {:b 2}} [:a] (fn [n] 1))`> = '{:a 1}'
const lisptestupdatein2ac: Cion.Lisp<`(update-in {:a {:b 2}} [:c] inc)`> = 'nil'
const lisptestupdatein2ad: Cion.Lisp<`(update-in {:a [2]} [:a 0] inc)`> = '{:a [3]}'
const lisptestupdatein2ae: Cion.Lisp<`(update-in [9 {:a [2]}] [1 :a 0] inc)`> = '[9 {:a [3]}]'
const lisptestupdatein2af: Cion.Lisp<"(update-in {:a 1 :b 2} [:a :c] inc)"> = 'nil'
