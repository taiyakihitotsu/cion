import type {SEncoder} from '../../src/compiler/index.js'
import type {Equal} from '../../src/util.js'

// -------------------------
// -- Encoder
// -------------------------

const test_s_encoder_prim_0: true = {} as Equal<'1', SEncoder<['prim', 1]>>
const test_s_encoder_prim_1: true = {} as Equal<'true', SEncoder<['prim', true]>>
const test_s_encoder_prim_2: true = {} as Equal<"'string'", SEncoder<['prim', 'string']>>

const test_s_encoder_map_0: true = {} as Equal<'{:a 1}', SEncoder<['map', [['key', ':a'], ['prim', 1]]]>>
const test_s_encoder_map_1: true = {} as Equal<'{:a 1 :b 2}', SEncoder<['map', [['key', ':a'], ['prim', 1], ['key', ':b'], ['prim', 2]]]>>
const test_s_encoder_map_2: true = {} as Equal<'{:a 1 :b 2 :c {:ca 3}}', SEncoder<['map', [['key', ':a'], ['prim', 1], ['key', ':b'], ['prim', 2], ['key', ':c'], ['map', [['key', ':ca'], ['prim', 3]]]]]>>

const test_s_encoder_vec_0: true = {} as Equal<'[1 2 3]', SEncoder<['vec', ['prim', 1], ['prim', 2], ['prim', 3]]>>
const test_s_encoder_vec_1: true = {} as Equal<'[1 [2 3]]', SEncoder<['vec', ['prim', 1], ['vec', ['prim', 2], ['prim', 3]]]>>
const test_s_encoder_vec_2: true = {} as Equal<'[]', SEncoder<['vec']>>

const test_s_encoder_mixed_0: true = {} as Equal<'[1 {:a 2}]', SEncoder<['vec', ['prim', 1], ['map', [['key', ':a'], ['prim', 2]]]]>>
const test_s_encoder_mixed_1: true = {} as Equal<'{:a 2 :b [3 4]}', SEncoder<['map', [['key', ':a'], ['prim', 2], ['key', ':b'], ['vec', ['prim', 3], ['prim', 4]]]]>>

const test_s_encoder_if_0: true = {} as Equal<'(if true 1 2)', SEncoder<['if', ['prim', true], ['prim', 1], ['prim', 2]]>>
const test_s_encoder_if_1: true = {} as Equal<'(if true 1)', SEncoder<['if', ['prim', true], ['prim', 1]]>>

const test_s_encoder_let_0: true = {} as Equal<
  "(let [a 'text-a' b '/text-b'] (str a b))",
  SEncoder<[
    'let',
    [['sym', 'a'], ['prim', 'text-a'], ['sym', 'b'], ['prim', '/text-b']],
    [['sym', 'str'], ['sym', 'a'], ['sym', 'b']],
  ]>
>

const test_s_encoder_nested_let: true = {} as Equal<
  "(let [a 'text-a' b '/text-b'] (if (= (let [aa 1] (= aa 1)) true) (str a b) 1))",
  SEncoder<[
    'let',
    [['sym', 'a'], ['prim', 'text-a'], ['sym', 'b'], ['prim', '/text-b']],
    ['if', [['sym', '='], ['let', [['sym', 'aa'], ['prim', 1]], [['sym', '='], ['sym', 'aa'], ['prim', 1]]], ['prim', true]], [['sym', 'str'], ['sym', 'a'], ['sym', 'b']], ['prim', 1]],
  ]>>
