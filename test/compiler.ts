import type * as Compiler from '../src/compiler.js'
import type {Equal} from '../src/util.js'

const test_compiler_rec_0: true = {} as Equal<
  ['(', 'x', '(', '(', 'if', 'a', 'b', 'c', ')', 'y', ')', ')'],
  Compiler.Rec<Compiler.recp<' (x ((if a b c) y))'>>
>

const test_compiler_rec_1: true = {} as Equal<
  ['(', 'x', '(', 'if', 'a', 'b', 'c', ')', 'y', ')'],
  Compiler.Rec<Compiler.recp<' (x (if a b c) y)'>>
>

const test_compiler_rec_2: true = {} as Equal<
  ['(', '(', 'f', ')', ')'],
  Compiler.Rec<Compiler.recp<' ((f))'>>
>

const test_compiler_rec_3: true = {} as Equal<
  ['(', '(', '(', '(', '(', '(', 'x', ')', ')', ')', ')', ')', ')'],
  Compiler.Rec<Compiler.recp<' ((((((x))))))'>>
>

const test_compiler_rec_4: true = {} as Equal<
  ['(', 'let', '[', 'a', '1', 'b', '2', ']', '(', 'if', 'true', 't', 'f', ')', ')'],
  Compiler.Rec<Compiler.recp<' (let [a 1 b 2] (if true t f))'>>
>

const test_compiler_rec_5: true = {} as Equal<
  ['(', 'let', '[', 'a', '"test is this"', ']', '(', 'str', '"a b"', 'a', ')', ')'],
  Compiler.Rec<Compiler.recp<` (let [a "test is this"] (str "a b" a))`>>
>

const test_compiler_rec_6: true = {} as Equal<
  ['30'],
  Compiler.Rec<Compiler.recp<` 30`>>
>

const test_compiler_rec_7: true = {} as Equal<
  ['[', '30', ']'],
  Compiler.Rec<Compiler.recp<` [30]`>>
>

const test_compiler_rec_8: true = {} as Equal<
  ['[', '3/2', ']'],
  Compiler.Rec<Compiler.recp<`      [3/2]`>>
>

const test_compiler_rec_9: true = {} as Equal<
  ['[', '3/2', ']'],
  Compiler.Rec<Compiler.recp<`   [3/2]`>>
>

// -------------------------------
// --- hash map ---
// -------------------------------
const test_compiler_rec_10: true = {} as Equal<
  ['(', 'let', '[', 'a', '{', ':a', '1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')'],
  Compiler.Rec<Compiler.recp<' (let [a {:a 1 :b 2}] (> (:a a) (:b a)))'>>
>

const test_compiler_rec_11: true = {} as Equal<
  ['(', 'let', '[', 'a', '{', ':a', '-1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')'],
  Compiler.Rec<Compiler.recp<' (let [a {:a -1 :b 2}] (> (:a a) (:b a)))'>>
>
// -------------------------
// -- Regex
// -------------------------
type email = `'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'`
const test_compiler_rec_email: true = {} as Equal<
  [`'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'`],
  Compiler.Rec<Compiler.recp<` ${email}`>>
>

// NOTE (A) : they spit a 2589 error with LegacyCompiler.SParser.
type crossX = '(fn [m0 m1] (>= (+ (:x m0) (:w m0)) (:x m1)))'
type m0 = '{:x 0 :w 1}'
type m1 = '{:x 1 :w 2}'
type aa = `(or (${crossX} ${m0} ${m1}) (:x {:x false}))`
type aaa = `(or (${crossX} ${m0} ${m1}) (${crossX} ${m1} ${m0}))`
type aaaa = ['(', 'or', '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '0', ':w', '1', '}', '{', ':x', '1', ':w', '2', '}', ')',  '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '1', ':w', '2', '}', '{', ':x', '0', ':w', '1', '}', ')', ')']
const test_compiler_rec_complex_or: true = {} as Equal<
  ['(', 'or', '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '0', ':w', '1', '}', '{', ':x', '1', ':w', '2', '}', ')',  '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '1', ':w', '2', '}', '{', ':x', '0', ':w', '1', '}', ')', ')'],
  aaaa
>

type tesa = Compiler.Rec<Compiler.recp<Compiler.SPad<aaa>>>
const test_compiler_rec_tesa: true = {} as Equal<
  ['(', 'or', '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '0', ':w', '1', '}', '{', ':x', '1', ':w', '2', '}', ')',  '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '1', ':w', '2', '}', '{', ':x', '0', ':w', '1', '}', ')', ')'],
  tesa
>

const test_s_parser_0: true = {} as Equal<
  ['(', 'x', '(', '(', 'if', 'a', 'b', 'c', ')', 'y', ')', ')'],
  Compiler.SParser<' (x ((if a b c) y))'>
>

const test_s_parser_1: true = {} as Equal<
  ['(', 'x', '(', 'if', 'a', 'b', 'c', ')', 'y', ')'],
  Compiler.SParser<' (x (if a b c) y)'>
>

const test_s_parser_2: true = {} as Equal<
  ['(', '(', 'f', ')', ')'],
  Compiler.SParser<' ((f))'>
>

const test_s_parser_3: true = {} as Equal<
  ['(', '(', '(', '(', '(', '(', 'x', ')', ')', ')', ')', ')', ')'],
  Compiler.SParser<' ((((((x))))))'>
>

const test_s_parser_4: true = {} as Equal<
  ['(', 'let', '[', 'a', '1', 'b', '2', ']', '(', 'if', 'true', 't', 'f', ')', ')'],
  Compiler.SParser<' (let [a 1 b 2] (if true t f))'>
>

const test_s_parser_5: true = {} as Equal<
  ['(', 'let', '[', 'a', '"test is this"', ']', '(', 'str', '"a b"', 'a', ')', ')'],
  Compiler.SParser<' (let [a "test is this"] (str "a b" a))'>
>

const test_s_parser_map_0: true = {} as Equal<
  ['(', 'let', '[', 'a', '{', ':a', '1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')'],
  Compiler.SParser<' (let [a {:a 1 :b 2}] (> (:a a) (:b a)))'>
>

const test_s_parser_map_1: true = {} as Equal<
  ['(', 'let', '[', 'a', '{', ':a', '-1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')'],
  Compiler.SParser<' (let [a {:a -1 :b 2}] (> (:a a) (:b a)))'>
>

// ---------------
// -- Rational
// ---------------

// [todo]
export type SIsNum<
  S
, Top extends boolean = true> =
  S extends `${infer H}${infer R}`
    ? H extends '-'
      ? Top extends true
        ? SIsNum<R, false> extends true
          ? true
        : false
      : false
    : H extends '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'
      ? R extends ''
        ? true
      : SIsNum<R, false>
    : false
  : false

const test_read_rational_0: true = {} as Equal<
  ['3', '2'],
  Compiler.ReadRational<`3/2`>
>

const test_read_rational_1: true = {} as Equal<
  ['-3', '2'],
  Compiler.ReadRational<`-3/2`>
>

const test_read_rational_2: true = {} as Equal<
  ['-3'],
  Compiler.ReadRational<`-3`>
>

const test_read_rational_3: true = {} as Equal<
  [],
  Compiler.ReadRational<`str`>
>

const test_read_rational_4: true = {} as Equal<
  [],
  Compiler.ReadRational<`2str`>
>

const test_read_rational_5: true = {} as Equal<
  [],
  Compiler.ReadRational<`3  /2`>
>

const test_s_compiler_0: true = {} as Equal<
  ['prim', '0000000000000010'],
  Compiler.SCompiler<['2']>
>

const test_s_compiler_1: true = {} as Equal<
  ['prim', ['0000000000000010', '0000000000000011']],
  Compiler.SCompiler<['2/3']>
>

const test_s_compiler_2: true = {} as Equal<
  [['sym', '+'], ['prim', '0000000000000010'], ['prim', ['0000000000000010', '0000000000000011']]],
  Compiler.SCompiler<['(', '+', '2', '2/3', ')']>
>

const test_s_compiler_3: true = {} as Equal<
  [['sym', '+'], ['prim', '0000000000000010'], ['prim', ['1111111111111110', '0000000000000011']]],
  Compiler.SCompiler<['(', '+', '2', '-2/3', ')']>
>

const test_s_compiler_4: true = {} as Equal<
  [['sym', '+'], ['prim', '0000000000000000'], [['sym', 'inc'], ['prim', '0000000000000001']]],
  Compiler.SCompiler<['(', '+', '0', '(', 'inc', '1', ')', ')']>
>

const test_s_compiler_5: true = {} as Equal<
  ['let', [['sym', 'a'], ['prim', '0000000000000001']], ['if', ['prim', true], ['sym', 't'], ['sym', 'f']]],
  Compiler.SCompiler<['(', 'let', '[', 'a', '1', ']', '(', 'if', 'true', 't', 'f', ')', ')']>
>

const test_s_compiler_6: true = {} as Equal<
  [['sym', '+'], ['prim', '1111111111111111'], ['prim', '0000000000000010']],
  Compiler.SCompiler<['(', '+', '-1', '2', ')']>
>

const test_s_compiler_string: true = {} as Equal<
  ['let', [['sym', 'a'], ['prim', '"aaa bbb"']]],
  Compiler.SCompiler<['(', 'let', '[', 'a', '"aaa bbb"', ']', ')']>
>

const test_s_compiler_map_0: true = {} as Equal<
  ['map', [['key', ':a'], ['prim', '0000000000000001'], ['key', ':b'], ['prim', '0000000000000010']]],
  Compiler.SCompiler<['{', ':a', '01', ':b', '2', '}']>
>

const test_s_compiler_map_1: true = {} as Equal<
  ['map', [['key', ':a'], ['prim', '0000000000000001'], ['key', ':b'], ['prim', '0000000000000010'], ['key', ':c'], ['map', [['key', ':c1'], ['prim', '0000000000000101']]]]],
  Compiler.SCompiler<['{', ':a', '01', ':b', '2', ':c', '{', ':c1', '5', '}', '}']>
>

const test_s_compiler_map_nil: true = {} as Equal<
  ['map', [['key', ':a'], ['prim', '0000000000000001'], ['key', ':b'], ['prim', '0000000000000010'], ['key', ':c'], ['map', [['key', ':c1'], ['prim', 'nil']]]]],
  Compiler.SCompiler<['{', ':a', '01', ':b', '2', ':c', '{', ':c1', 'nil', '}', '}']>
>
const test_s_encoder_prim_0: true = {} as Equal<'1', Compiler.SEncoder<['prim', 1]>>
const test_s_encoder_prim_1: true = {} as Equal<'true', Compiler.SEncoder<['prim', true]>>
const test_s_encoder_prim_2: true = {} as Equal<"'string'", Compiler.SEncoder<['prim', 'string']>>

const test_s_encoder_map_0: true = {} as Equal<'{:a 1}', Compiler.SEncoder<['map', [['key', ':a'], ['prim', 1]]]>>
const test_s_encoder_map_1: true = {} as Equal<'{:a 1 :b 2}', Compiler.SEncoder<['map', [['key', ':a'], ['prim', 1], ['key', ':b'], ['prim', 2]]]>>
const test_s_encoder_map_2: true = {} as Equal<'{:a 1 :b 2 :c {:ca 3}}', Compiler.SEncoder<['map', [['key', ':a'], ['prim', 1], ['key', ':b'], ['prim', 2], ['key', ':c'], ['map', [['key', ':ca'], ['prim', 3]]]]]>>

const test_s_encoder_vec_0: true = {} as Equal<'[1 2 3]', Compiler.SEncoder<['vec', ['prim', 1], ['prim', 2], ['prim', 3]]>>
const test_s_encoder_vec_1: true = {} as Equal<'[1 [2 3]]', Compiler.SEncoder<['vec', ['prim', 1], ['vec', ['prim', 2], ['prim', 3]]]>>
const test_s_encoder_vec_2: true = {} as Equal<'[]', Compiler.SEncoder<['vec']>>

const test_s_encoder_mixed_0: true = {} as Equal<'[1 {:a 2}]', Compiler.SEncoder<['vec', ['prim', 1], ['map', [['key', ':a'], ['prim', 2]]]]>>
const test_s_encoder_mixed_1: true = {} as Equal<'{:a 2 :b [3 4]}', Compiler.SEncoder<['map', [['key', ':a'], ['prim', 2], ['key', ':b'], ['vec', ['prim', 3], ['prim', 4]]]]>>

const test_s_encoder_if_0: true = {} as Equal<'(if true 1 2)', Compiler.SEncoder<['if', ['prim', true], ['prim', 1], ['prim', 2]]>>
const test_s_encoder_if_1: true = {} as Equal<'(if true 1)', Compiler.SEncoder<['if', ['prim', true], ['prim', 1]]>>

const test_s_encoder_let_0: true = {} as Equal<
  "(let [a 'text-a' b '/text-b'] (str a b))",
  Compiler.SEncoder<[
    'let',
    [['sym', 'a'], ['prim', 'text-a'], ['sym', 'b'], ['prim', '/text-b']],
    [['sym', 'str'], ['sym', 'a'], ['sym', 'b']],
  ]>
>

const test_s_encoder_nested_let: true = {} as Equal<
  "(let [a 'text-a' b '/text-b'] (if (= (let [aa 1] (= aa 1)) true) (str a b) 1))",
  Compiler.SEncoder<[
    'let',
    [['sym', 'a'], ['prim', 'text-a'], ['sym', 'b'], ['prim', '/text-b']],
    ['if', [['sym', '='], ['let', [['sym', 'aa'], ['prim', 1]], [['sym', '='], ['sym', 'aa'], ['prim', 1]]], ['prim', true]], [['sym', 'str'], ['sym', 'a'], ['sym', 'b']], ['prim', 1]],
  ]>
>

const test_s_compiler_nested_str: true = {} as Equal<
  [['sym', 'str'], ['prim', "'a'"], [['sym', 'str'], ['prim', "'s1'"], ['prim', "'s2'"]]],
  Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"(str 'a' (str 's1' 's2'))">>>
>

const test_s_compiler_vec_simple: true = {} as Equal<
  ['vec', ['prim', "'a'"]],
  Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"['a']">>>
>

const test_s_compiler_math_binary: true = {} as Equal<
  [['sym', '+'], ['prim', '0000000000000001'], [['sym', '+'], ['prim', '0000000000001010'], ['prim', '0000000000001011']]],
  Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"(+ 01 (+ 10 11))">>>
>

const test_s_compiler_vec_nest_0: true = {} as Equal<
  ['vec', ['prim', '0000000000000100'], ['prim', '0000000000000011']],
  Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"[4 3]">>>
>

const test_s_compiler_vec_nest_1: true = {} as Equal<
  ['vec', ['prim', '0000000000000100'], ['prim', '0000000000000011'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000001']]],
  Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"[4 3 [2 1]]">>>
>

const test_s_compiler_let_vec: true = {} as Equal<
  ['let', [['sym', 'a'], ['vec', ['prim', '0000000000000100'], ['prim', '0000000000000011'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000001']]]], ['sym', 'a']],
  Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"(let [a [4 3 [2 1]]] a)">>>
>

const test_s_compiler_empty_vec: true = {} as Equal<
  [['sym', 'first'], ['vec']],
  Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"(first [])">>>
>

const test_s_compiler_mixed_let: true = {} as Equal<
  ['let', [['sym', 'x'], ['map', [['key', ':a'], ['prim', "'a'"]]], ['sym', 'y'], ['prim', "'a'"], ['sym', 'z'], ['vec', ['prim', "'a'"]]]],
  Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"(let [x {:a 'a'} y 'a' z ['a']])">>>
>

// -- -----------
// -- Unparser
// -- -----------
const test_unparse_prim_0: true = {} as Equal<'0', Compiler.Unparse<['prim', '0']>>
const test_unparse_prim_1: true = {} as Equal<"'str'", Compiler.Unparse<['prim', "'str'"]>>
const test_unparse_sym: true = {} as Equal<'x', Compiler.Unparse<['sym', 'x']>>

const test_unparse_fn_0: true = {} as Equal<
  '(fn [x y] (+ x y))',
  Compiler.Unparse<['fn', [['sym', 'x'], ['sym', 'y']], [['sym', '+'], ['sym', 'x'], ['sym', 'y']]]>
>
const test_unparse_fn_1: true = {} as Equal<
  '(fn [x y] 1)',
  Compiler.Unparse<['fn', [['sym', 'x'], ['sym', 'y']], ['prim', '1']]>
>
const test_unparse_fn_call: true = {} as Equal<
  '((fn [x y] (+ x y)) 2 3)',
  Compiler.Unparse<[['fn', [['sym', 'x'], ['sym', 'y']], [['sym', '+'], ['sym', 'x'], ['sym', 'y']]], ['prim', '2'], ['prim', '3']]>
>

const test_unparse_if_0: true = {} as Equal<
  '(if true 0)',
  Compiler.Unparse<['if', ['prim', true], ['prim', '0']]>
>
const test_unparse_if_1: true = {} as Equal<
  '(if true 0 1)',
  Compiler.Unparse<['if', ['prim', true], ['prim', '0'], ['prim', '1']]>
>
const test_unparse_let_if: true = {} as Equal<
  '(let [a 1] (if true 0 1))',
  Compiler.Unparse<['let', [['sym', 'a'], ['prim', '1']], ['if', ['prim', true], ['prim', '0'], ['prim', '1']]]>
>
const test_unparse_let_if_fn: true = {} as Equal<
  '(let [a 1] (if true 0 (fn [a b] 0)))',
  Compiler.Unparse<['let', [['sym', 'a'], ['prim', '1']], ['if', ['prim', true], ['prim', '0'], ['fn', [['sym', 'a'], ['sym', 'b']], ['prim', '0']]]]>
>

const test_unparse_let_simple: true = {} as Equal<
  '(let [a 1] 1)',
  Compiler.Unparse<['let', [['sym', 'a'], ['prim', '1']], ['prim', '1']]>
>
const test_unparse_let_nested: true = {} as Equal<
  '(let [b 2] (let [a 1] 1))',
  Compiler.Unparse<['let', [['sym', 'b'], ['prim', '10']], ['let', [['sym', 'a'], ['prim', '1']], ['prim', '1']]]>
>

const test_unparse_vec_0: true = {} as Equal<'[0 1]', Compiler.Unparse<['vec', ['prim', '0'], ['prim', '1']]>>
const test_unparse_vec_1: true = {} as Equal<'[:a 1]', Compiler.Unparse<['vec', ['key', ':a'], ['prim', '1']]>>
const test_unparse_vec_nested: true = {} as Equal<'[:a 1 [1]]', Compiler.Unparse<['vec', ['key', ':a'], ['prim', '1'], ['vec', ['prim', '01']]]>>
const test_unparse_vec_empty: true = {} as Equal<'[]', Compiler.Unparse<['vec']>>
const test_unparse_vec_mixed_empty: true = {} as Equal<'[0 []]', Compiler.Unparse<['vec', ['prim', '0'], ['vec']]>>
const test_unparse_vec_recursive_empty: true = {} as Equal<'[[] []]', Compiler.Unparse<['vec', ['vec'], ['vec']]>>

const test_unparse_map_0: true = {} as Equal<'{:a 0}', Compiler.Unparse<['map', [['key', ':a'], ['prim', '0']]]>>
const test_unparse_map_1: true = {} as Equal<'{:a 0 :b 1}', Compiler.Unparse<['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1']]]>>
const test_unparse_map_nested: true = {} as Equal<
  '{:a 0 :b 1 :c {:d 2}}',
  Compiler.Unparse<['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1'], ['key', ':c'], ['map', [['key', ':d'], ['prim', '10']]]]]>
>
