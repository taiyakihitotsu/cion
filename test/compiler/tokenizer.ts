import type {Tokenizer, SCompiler} from '../../src/compiler/index.js'
import type {Equal} from '../../src/util.js'

// ---------------------
// -- Parser
// ---------------------

const test_parser_0: true = {} as Equal<
  ['(', 'x', '(', '(', 'if', 'a', 'b', 'c', ')', 'y', ')', ')'],
  Tokenizer<'(x ((if a b c) y))'>>

const test_parser_1: true = {} as Equal<
  ['(', 'x', '(', 'if', 'a', 'b', 'c', ')', 'y', ')'],
  Tokenizer<'(x (if a b c) y)'>>


const test_parser_2: true = {} as Equal<
  ['(', '(', 'f', ')', ')'],
  Tokenizer<'((f))'>>

const test_parser_3: true = {} as Equal<
  ['(', '(', '(', '(', '(', '(', 'x', ')', ')', ')', ')', ')', ')'],
  Tokenizer<'((((((x))))))'>>

const test_parser_4: true = {} as Equal<
  ['(', 'let', '[', 'a', '1', 'b', '2', ']', '(', 'if', 'true', 't', 'f', ')', ')'],
  Tokenizer<'(let [a 1 b 2] (if true t f))'>>

const test_parser_5: true = {} as Equal<
  ['(', 'let', '[', 'a', '"test is this"', ']', '(', 'str', '"a b"', 'a', ')', ')'],
  Tokenizer<`(let [a "test is this"] (str "a b" a))`>>

const test_parser_6: true = {} as Equal<
  ['30'],
  Tokenizer<`30`>>

const test_parser_7: true = {} as Equal<
  ['[', '30', ']'],
  Tokenizer<`[30]`>>

const test_parser_8: true = {} as Equal<
  ['[', '3/2', ']'],
  Tokenizer<`[3/2]`>>

const test_parser_9: true = {} as Equal<
  ['[', '3/2', ']'],
  Tokenizer<`   [3/2]`>>

// -------------------------------
// --- hash map ---
// -------------------------------

const test_compiler_rec_10: true = {} as Equal<
  ['(', 'let', '[', 'a', '{', ':a', '1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')'],
  Tokenizer<'(let [a {:a 1 :b 2}] (> (:a a) (:b a)))'>>

const test_compiler_rec_11: true = {} as Equal<
  ['(', 'let', '[', 'a', '{', ':a', '-1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')'],
  Tokenizer<'(let [a {:a -1 :b 2}] (> (:a a) (:b a)))'>>

// -------------------------
// -- Regex
// -------------------------

type email = `'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'`
const test_compiler_rec_email: true = {} as Equal<
  [`'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'`],
  Tokenizer<`${email}`>>

// NOTE (A) : they spit a 2589 error with LegacyTokenizer.
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

type tesa = Tokenizer<aaa>
const test_compiler_rec_tesa: true = {} as Equal<
  ['(', 'or', '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '0', ':w', '1', '}', '{', ':x', '1', ':w', '2', '}', ')',  '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '1', ':w', '2', '}', '{', ':x', '0', ':w', '1', '}', ')', ')'],
  tesa
>

const test_s_parser_0: true = {} as Equal<
  ['(', 'x', '(', '(', 'if', 'a', 'b', 'c', ')', 'y', ')', ')'],
  Tokenizer<'(x ((if a b c) y))'>
>

const test_s_parser_1: true = {} as Equal<
  ['(', 'x', '(', 'if', 'a', 'b', 'c', ')', 'y', ')'],
  Tokenizer<'(x (if a b c) y)'>
>

const test_s_parser_2: true = {} as Equal<
  ['(', '(', 'f', ')', ')'],
  Tokenizer<'((f))'>
>

const test_s_parser_3: true = {} as Equal<
  ['(', '(', '(', '(', '(', '(', 'x', ')', ')', ')', ')', ')', ')'],
  Tokenizer<'((((((x))))))'>
>

const test_s_parser_4: true = {} as Equal<
  ['(', 'let', '[', 'a', '1', 'b', '2', ']', '(', 'if', 'true', 't', 'f', ')', ')'],
  Tokenizer<'(let [a 1 b 2] (if true t f))'>
>

const test_s_parser_5: true = {} as Equal<
  ['(', 'let', '[', 'a', '"test is this"', ']', '(', 'str', '"a b"', 'a', ')', ')'],
  Tokenizer<'(let [a "test is this"] (str "a b" a))'>
>

const test_s_parser_map_0: true = {} as Equal<
  ['(', 'let', '[', 'a', '{', ':a', '1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')'],
  Tokenizer<'(let [a {:a 1 :b 2}] (> (:a a) (:b a)))'>
>

const test_s_parser_map_1: true = {} as Equal<
  ['(', 'let', '[', 'a', '{', ':a', '-1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')'],
  Tokenizer<'(let [a {:a -1 :b 2}] (> (:a a) (:b a)))'>
>


const test_s_compiler_nested_str: true = {} as Equal<
  [['sym', 'str'], ['prim', "'a'"], [['sym', 'str'], ['prim', "'s1'"], ['prim', "'s2'"]]],
  SCompiler<Tokenizer<"(str 'a' (str 's1' 's2'))">>>

const test_s_compiler_vec_simple: true = {} as Equal<
  ['vec', ['prim', "'a'"]],
  SCompiler<Tokenizer<"['a']">>>

const test_s_compiler_math_binary: true = {} as Equal<
  [['sym', '+'], ['prim', '0000000000000001'], [['sym', '+'], ['prim', '0000000000001010'], ['prim', '0000000000001011']]],
  SCompiler<Tokenizer<"(+ 01 (+ 10 11))">>>

const test_s_compiler_vec_nest_0: true = {} as Equal<
  ['vec', ['prim', '0000000000000100'], ['prim', '0000000000000011']],
  SCompiler<Tokenizer<"[4 3]">>>

const test_s_compiler_vec_nest_1: true = {} as Equal<
  ['vec', ['prim', '0000000000000100'], ['prim', '0000000000000011'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000001']]],
  SCompiler<Tokenizer<"[4 3 [2 1]]">>>

const test_s_compiler_let_vec: true = {} as Equal<
  ['let', [['sym', 'a'], ['vec', ['prim', '0000000000000100'], ['prim', '0000000000000011'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000001']]]], ['sym', 'a']],
  SCompiler<Tokenizer<"(let [a [4 3 [2 1]]] a)">>>

const test_s_compiler_empty_vec: true = {} as Equal<
  [['sym', 'first'], ['vec']],
  SCompiler<Tokenizer<"(first [])">>>

const test_s_compiler_mixed_let: true = {} as Equal<
  ['let', [['sym', 'x'], ['map', [['key', ':a'], ['prim', "'a'"]]], ['sym', 'y'], ['prim', "'a'"], ['sym', 'z'], ['vec', ['prim', "'a'"]]]],
  SCompiler<Tokenizer<"(let [x {:a 'a'} y 'a' z ['a']])">>>
