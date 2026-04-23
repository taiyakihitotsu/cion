import type {Unparse} from '../../src/compiler/index.js'
import type {Equal} from '../../src/util.js'

// ------------------------
// -- Unparser
// ------------------------

const test_unparse_prim_0: true = {} as Equal<'0', Unparse<['prim', '0']>>
const test_unparse_prim_1: true = {} as Equal<"'str'", Unparse<['prim', "'str'"]>>
const test_unparse_sym: true = {} as Equal<'x', Unparse<['sym', 'x']>>

const test_unparse_fn_0: true = {} as Equal<
  '(fn [x y] (+ x y))',
  Unparse<['fn', [['sym', 'x'], ['sym', 'y']], [['sym', '+'], ['sym', 'x'], ['sym', 'y']]]>
>
const test_unparse_fn_1: true = {} as Equal<
  '(fn [x y] 1)',
  Unparse<['fn', [['sym', 'x'], ['sym', 'y']], ['prim', '1']]>
>
const test_unparse_fn_call: true = {} as Equal<
  '((fn [x y] (+ x y)) 2 3)',
  Unparse<[['fn', [['sym', 'x'], ['sym', 'y']], [['sym', '+'], ['sym', 'x'], ['sym', 'y']]], ['prim', '2'], ['prim', '3']]>
>

const test_unparse_if_0: true = {} as Equal<
  '(if true 0)',
  Unparse<['if', ['prim', true], ['prim', '0']]>
>
const test_unparse_if_1: true = {} as Equal<
  '(if true 0 1)',
  Unparse<['if', ['prim', true], ['prim', '0'], ['prim', '1']]>
>
const test_unparse_let_if: true = {} as Equal<
  '(let [a 1] (if true 0 1))',
  Unparse<['let', [['sym', 'a'], ['prim', '1']], ['if', ['prim', true], ['prim', '0'], ['prim', '1']]]>
>
const test_unparse_let_if_fn: true = {} as Equal<
  '(let [a 1] (if true 0 (fn [a b] 0)))',
  Unparse<['let', [['sym', 'a'], ['prim', '1']], ['if', ['prim', true], ['prim', '0'], ['fn', [['sym', 'a'], ['sym', 'b']], ['prim', '0']]]]>
>

const test_unparse_let_simple: true = {} as Equal<
  '(let [a 1] 1)',
  Unparse<['let', [['sym', 'a'], ['prim', '1']], ['prim', '1']]>
>
const test_unparse_let_nested: true = {} as Equal<
  '(let [b 2] (let [a 1] 1))',
  Unparse<['let', [['sym', 'b'], ['prim', '10']], ['let', [['sym', 'a'], ['prim', '1']], ['prim', '1']]]>
>

const test_unparse_vec_0: true = {} as Equal<'[0 1]', Unparse<['vec', ['prim', '0'], ['prim', '1']]>>
const test_unparse_vec_1: true = {} as Equal<'[:a 1]', Unparse<['vec', ['key', ':a'], ['prim', '1']]>>
const test_unparse_vec_nested: true = {} as Equal<'[:a 1 [1]]', Unparse<['vec', ['key', ':a'], ['prim', '1'], ['vec', ['prim', '01']]]>>
const test_unparse_vec_empty: true = {} as Equal<'[]', Unparse<['vec']>>
const test_unparse_vec_mixed_empty: true = {} as Equal<'[0 []]', Unparse<['vec', ['prim', '0'], ['vec']]>>
const test_unparse_vec_recursive_empty: true = {} as Equal<'[[] []]', Unparse<['vec', ['vec'], ['vec']]>>

const test_unparse_map_0: true = {} as Equal<'{:a 0}', Unparse<['map', [['key', ':a'], ['prim', '0']]]>>
const test_unparse_map_1: true = {} as Equal<'{:a 0 :b 1}', Unparse<['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1']]]>>
const test_unparse_map_nested: true = {} as Equal<
  '{:a 0 :b 1 :c {:d 2}}',
  Unparse<['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1'], ['key', ':c'], ['map', [['key', ':d'], ['prim', '10']]]]]>
>
