import type Cion from '../../src/index.js'
import type { Eval } from '../../src/index.js'
import type { Sexpr, Vector, Each } from '../../src/sexprtypes.js'
import type { Equal } from '../../src/util.js'

// --- Type Compatibility Tests ---

type testvecvec0 = [[['key', ':a'], ['map', [['key', ':a'], ['prim', `'-'`]]]], ['vec', ['prim', `'-'`]]]
type testvecvec1 = ['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', `'-'`]]]], ['prim', '0']]
type testvecvec2 = ['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', `'-'`]]]], ['prim', '0']]

// --- Eval (Internal Tuple Evaluation) Tests ---

const eval_test_0: true = {} as Equal<
  ['vec', ['prim', `'-'`], ['prim', '0']],
  Eval<['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', `'-'`]]]], ['prim', '0']]>
>

const eval_test_1: true = {} as Equal<
  ['vec', ['prim', '0001']],
  Eval<['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', '0001']]]]]>
>

const eval_test_2: true = {} as Equal<
  ['vec', ['prim', '0001'], ['prim', '0001']],
  Eval<['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', '0001']]]], ['prim', '0001']]>
>

const eval_test_3: true = {} as Equal<
  ['vec', ['prim', '0001'], ['prim', '0001'], ['prim', '0001']],
  Eval<['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', '0001']]]], ['prim', '0001'], ['prim', '0001']]>
>

const eval_test_4: true = {} as Equal<['sym', 'x'], Eval<['sym', 'x']>>
const eval_test_5: true = {} as Equal<['prim', '0'], Eval<['prim', '0']>>

// --- Vector Intersection Type Tests ---

type vectortest = [`vec`, [`prim`, `1`]]
const v_test_0: true = {} as Equal<[`vec`, [`prim`, `1`]], vectortest>

type vectortest2 = [`vec`, [`prim`, `1`], [`prim`, `2`]]
const v_test_1: true = {} as Equal<[`vec`, [`prim`, `1`], [`prim`, `2`]], vectortest2>

type vectortest3 = [`vec`, [`vec`, [`prim`, `2`]]]
const v_test_2: true = {} as Equal<[`vec`, [`vec`, [`prim`, `2`]]], vectortest3>

type vectortest4 = [`vec`, [`vec`, [`vec`, [`prim`, true], [`prim`, `1`]], [`prim`, `1`]]]
const v_test_3: true = {} as Equal<[`vec`, [`vec`, [`vec`, [`prim`, true], [`prim`, `1`]], [`prim`, `1`]]], vectortest4>

// --- RawLisp (AST Generation) Tests ---

const raw_v_test_0: true = {} as Equal<
  ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']],
  Cion.RawLisp<"[0 1 2 3]">
>

const raw_v_test_1: true = {} as Equal<
  ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']],
  Cion.RawLisp<"(vector 0 1 2 3)">
>

// --- Lisp (String Result) Tests ---

const lisp_v_test_0: true = {} as Equal<'[0]',     Cion.Lisp<`(vector 0)`>>
const lisp_v_test_1: true = {} as Equal<'[0 1 2]', Cion.Lisp<`(vector 0 1 2)`>>
