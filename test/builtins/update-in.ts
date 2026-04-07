import type Cion from '../../src/index.js'
import type { _UpdateIn } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// --- Internal _UpdateIn Type Tests (AST / Tuple Level) ---

// Vector update at index 1
const update_in_int_0: true = {} as Equal<
  ['vec', ['prim', '0'], ['prim', ['0000000000001001', '0000000000000001']], ['prim', '10'], ['prim', '11'], ['prim', '100'], ['prim', '101']],
  _UpdateIn<['vec', ['prim', '0'], ['prim', '1'], ['prim', '10'], ['prim', '11'], ['prim', '100'], ['prim', '101']], ['vec', ['prim', '1']], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]>
>

// Map update at key :a
const update_in_int_1: true = {} as Equal<
  ['map', [['key', ':a'], ['prim', ['0000000000001001', '0000000000000001']], ['key', ':b'], ['prim', '10'], ['key', ':c'], ['prim', '11']]],
  _UpdateIn<['map', [['key', ':a'], ['prim', '1'], ['key', ':b'], ['prim', '10'], ['key', ':c'], ['prim', '11']]], ['vec', ['key', ':a']], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]>
>

// Nested structures update
const update_in_int_2: true = {} as Equal<
  ['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', ['0000000000001001', '0000000000000001']]], ['key', ':c'], ['prim', 2]],
  _UpdateIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', '0001']], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '11'], ['prim', '01']], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]>
>

// Deep nested structure (vec -> map -> map)
const update_in_int_3: true = {} as Equal<
  ['vec', ['prim', '11'], ['prim', '"d"'], ['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1'], ['key', ':c'], ['map', [['key', ':ca'], ['prim', ['0000000000001010', '0000000000000001']]]]]]],
  _UpdateIn<['vec', ['prim', '11'], ['prim', '"d"'], ['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1'], ['key', ':c'], ['map', [['key', ':ca'], ['prim', '10']]]]]], ['vec', ['prim', '10'], ['key', ':c'], ['key', ':ca']], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]>
>

// --- Error/Access Failed Handling ---

const update_in_error_0: true = {} as Equal<
  'AccessFailed',
  _UpdateIn<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['vec', ['key', ':ba'], ['prim', 33]], ['key', ':c'], ['prim', 2]], ['vec', ['prim', '11'], ['prim', '10']], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]>
>

// --- RawLisp Macro Expansion Tests ---

const update_in_raw_0: true = {} as Equal<
  ['vec', ['prim', '0000000000000000'], ['prim', ['0000000001100100', '0000000000000001']], ['prim', '0000000000000010']],
  Cion.RawLisp<'(update-in [0 1 2] [1] (fn [x] (+ x 99)))'>
>
const update_in_raw_1: true = {} as Equal<['prim', 'nil'], Cion.RawLisp<'(update-in [0 1 2] [99] (fn [x] (+ x 99)))'>>
const update_in_raw_2: true = {} as Equal<['prim', 'nil'], Cion.RawLisp<'(update-in [0 1 2] [99 99] (fn [x] (+ x 99)))'>>

// --- Lisp (String Result) Tests ---

const update_in_lisp_0: true = {} as Equal<"[2]",    Cion.Lisp<`(update-in [[2]] [0] first)`>>
const update_in_lisp_1: true = {} as Equal<'[2]',   Cion.Lisp<`(update-in [[2]] [0] (fn [n] (first n)))`>>
const update_in_lisp_2: true = {} as Equal<'[[3]]', Cion.Lisp<`(update-in [[2]] [0 0] inc)`>>
const update_in_lisp_3: true = {} as Equal<'[[3]]', Cion.Lisp<`(update-in [[2]] [0 0] (fn [n] (inc n)))`>>
const update_in_lisp_4: true = {} as Equal<'[0 [3]]', Cion.Lisp<`(update-in [0 [2]] [1 0] inc)`>>

// Path failure cases
const update_in_lisp_fail_0: true = {} as Equal<'nil', Cion.Lisp<`(update-in [0 [2]] [1 0 2] inc)`>>
const update_in_lisp_fail_1: true = {} as Equal<'nil', Cion.Lisp<`(update-in [0 {:a 2}] [1 :a 2] inc)`>>
const update_in_lisp_fail_2: true = {} as Equal<'nil', Cion.Lisp<`(update-in {:a [2]} [1 0 2] inc)`>>
const update_in_lisp_fail_3: true = {} as Equal<'nil', Cion.Lisp<`(update-in {:a [2]} [:a 2 9] inc)`>>
const update_in_lisp_fail_4: true = {} as Equal<'nil', Cion.Lisp<`(update-in {:a {:b 2}} [:c] inc)`>>
const update_in_lisp_fail_5: true = {} as Equal<'nil', Cion.Lisp<"(update-in {:a 1 :b 2} [:a :c] inc)">>

// Map specific updates
const update_in_lisp_map_0: true = {} as Equal<'{:a {:b 3}}', Cion.Lisp<`(update-in {:a {:b 2}} [:a :b] inc)`>>
const update_in_lisp_map_1: true = {} as Equal<'{:a 1}',       Cion.Lisp<`(update-in {:a {:b 2}} [:a] (fn [n] 1))`>>
const update_in_lisp_map_2: true = {} as Equal<'{:a [3]}',     Cion.Lisp<`(update-in {:a [2]} [:a 0] inc)`>>
const update_in_lisp_map_3: true = {} as Equal<'[9 {:a [3]}]', Cion.Lisp<`(update-in [9 {:a [2]}] [1 :a 0] inc)`>>
