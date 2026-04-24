import type {SCompiler} from '../../../src/compiler/index.js'
import type {Equal} from '../../../src/util.js'

const test_s_compiler_0: true = {} as Equal<
  ['prim', '0000000000000010'],
  SCompiler<['2']>>

const test_s_compiler_1: true = {} as Equal<
  ['prim', ['0000000000000010', '0000000000000011']],
  SCompiler<['2/3']>>

const test_s_compiler_2: true = {} as Equal<
  [['sym', '+'], ['prim', '0000000000000010'], ['prim', ['0000000000000010', '0000000000000011']]],
  SCompiler<['(', '+', '2', '2/3', ')']>>

const test_s_compiler_3: true = {} as Equal<
  [['sym', '+'], ['prim', '0000000000000010'], ['prim', ['1111111111111110', '0000000000000011']]],
  SCompiler<['(', '+', '2', '-2/3', ')']>>

const test_s_compiler_4: true = {} as Equal<
  [['sym', '+'], ['prim', '0000000000000000'], [['sym', 'inc'], ['prim', '0000000000000001']]],
  SCompiler<['(', '+', '0', '(', 'inc', '1', ')', ')']>>

const test_s_compiler_5: true = {} as Equal<
  ['let', [['sym', 'a'], ['prim', '0000000000000001']], ['if', ['prim', true], ['sym', 't'], ['sym', 'f']]],
  SCompiler<['(', 'let', '[', 'a', '1', ']', '(', 'if', 'true', 't', 'f', ')', ')']>>

const test_s_compiler_6: true = {} as Equal<
  [['sym', '+'], ['prim', '1111111111111111'], ['prim', '0000000000000010']],
  SCompiler<['(', '+', '-1', '2', ')']>>

const test_s_compiler_string: true = {} as Equal<
  ['let', [['sym', 'a'], ['prim', '"aaa bbb"']]],
  SCompiler<['(', 'let', '[', 'a', '"aaa bbb"', ']', ')']>>

const test_s_compiler_map_0: true = {} as Equal<
  ['map', [['key', ':a'], ['prim', '0000000000000001'], ['key', ':b'], ['prim', '0000000000000010']]],
  SCompiler<['{', ':a', '01', ':b', '2', '}']>>

const test_s_compiler_map_1: true = {} as Equal<
  ['map', [['key', ':a'], ['prim', '0000000000000001'], ['key', ':b'], ['prim', '0000000000000010'], ['key', ':c'], ['map', [['key', ':c1'], ['prim', '0000000000000101']]]]],
  SCompiler<['{', ':a', '01', ':b', '2', ':c', '{', ':c1', '5', '}', '}']>>

const test_s_compiler_map_nil: true = {} as Equal<
  ['map', [['key', ':a'], ['prim', '0000000000000001'], ['key', ':b'], ['prim', '0000000000000010'], ['key', ':c'], ['map', [['key', ':c1'], ['prim', 'nil']]]]],
  SCompiler<['{', ':a', '01', ':b', '2', ':c', '{', ':c1', 'nil', '}', '}']>>
