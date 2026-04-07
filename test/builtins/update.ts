import type Cion from '../../src/index.js'
import type { _Update } from '../../src/index.js'
import { AssocErrorMsg1 } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// --- Internal _Update Type Tests (AST / Tuple Level) ---

// Map update at key :a
const update_int_0: true = {} as Equal<
  ['map', [['key', ':a'], ['prim', ['0000000000001001', '0000000000000001']], ['key', ':b'], ['prim', '0001'], ['key', ':c'], ['prim', '0010']]],
  _Update<['map', [['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '0001'], ['key', ':c'], ['prim', '0010']]], ['key', ':a'], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]>>

// Vector update at index 3 (binary '11')
const update_int_1: true = {} as Equal<
  ['vec', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', ['0000000000001001', '0000000000000001']], ['key', ':c'], ['prim', '0010']],
  _Update<['vec', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '0001'], ['key', ':c'], ['prim', '0010']], ['prim', '11'], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]>>

// Error case: Key not found or invalid for vector
const update_int_error_0: true = {} as Equal<
  { error: 'AssocError1'; message: typeof AssocErrorMsg1; sexpr: ['vec', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '0001'], ['key', ':c'], ['prim', '0010']] },
  _Update<['vec', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '0001'], ['key', ':c'], ['prim', '0010']], ['key', ':d'], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]>>

// --- RawLisp Macro Expansion Tests ---

const update_raw_0: true = {} as Equal<
  ['vec', ['prim', '0000000000000000'], ['prim', ['0000000001100100', '0000000000000001']], ['prim', '0000000000000010']],
  Cion.RawLisp<'(update [0 1 2] 1 (fn [x] (+ x 99)))'>
>

const update_raw_1: true = {} as Equal<['prim', 'nil'], Cion.RawLisp<'(update [0 1 2] 99 (fn [x] (+ x 99)))'>>

const update_raw_2: true = {} as Equal<
  ['vec', ['prim', '0000000000000000'], ['prim', ['0000000000000010', '0000000000000001']], ['prim', '0000000000000010']],
  Cion.RawLisp<'(update [0 1 2] 1 inc)'>
>

// --- Lisp (String Result) Tests ---

const update_lisp_0: true = {} as Equal<'[0 100 2]', Cion.Lisp<`(update [0 1 2] 1 (fn [x] (+ x 99)))`>>
const update_lisp_1: true = {} as Equal<'nil',       Cion.Lisp<`(update [0 1 2] 99 (fn [x] (+ x 99)))`>>
const update_lisp_2: true = {} as Equal<'[0 2 2]',   Cion.Lisp<`(update [0 1 2] 1 inc)`>>

const update_lisp_3: true = {} as Equal<'{:a 100 :b 2}', Cion.Lisp<`(update {:a 1 :b 2} :a (fn [x] (+ x 99)))`>>
const update_lisp_4: true = {} as Equal<'{:a 2 :b 2}',   Cion.Lisp<`(update {:a 1 :b 2} :a inc)`>>

const update_lisp_fail_0: true = {} as Equal<'nil', Cion.Lisp<`(update {:a 1 :b 2} 1 (fn [x] (+ x 99)))`>>
const update_lisp_fail_1: true = {} as Equal<'nil', Cion.Lisp<`(update {:a 1 :b 2} 1 inc)`>>
