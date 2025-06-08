import type Compiler from '../src/compiler'

type xxxa =  Compiler.Rec<Compiler.recp<' )))]]'>>
type xa = Compiler.Rec<Compiler.recp<' (a or ((x y z {:a 11})))]]'>>
type xb = Compiler.Rec<Compiler.recp<' (or ((x y z {:a 11})))]]'>>
type xc = Compiler.Rec<Compiler.recp<Compiler.SPad<'(fn [m0 m1] (>= (+ (:x m0) (:w m0)) (:x m1)))'>>>

const recparseaaaaa: Compiler.Rec<Compiler.recp<' (x ((if a b c) y))'>> =
    ['(', 'x', '(', '(', 'if', 'a', 'b', 'c', ')', 'y', ')', ')']
const recparsebbbbb: Compiler.Rec<Compiler.recp<' (x (if a b c) y)'>> =
    ['(', 'x', '(', 'if', 'a', 'b', 'c', ')', 'y', ')']
const recparseccccc: Compiler.Rec<Compiler.recp<' ((f))'>> =
    ['(', '(', 'f', ')', ')']
const recparseddddd: Compiler.Rec<Compiler.recp<' ((((((x))))))'>> =
    ['(', '(', '(', '(', '(', '(', 'x', ')', ')', ')', ')', ')', ')']
const recparseeeeee: Compiler.Rec<Compiler.recp<' (let [a 1 b 2] (if true t f))'>> = ['(','let', '[', 'a', '1', 'b', '2', ']', '(', 'if', 'true', 't', 'f', ')', ')']
// const recparsestrtest0: Compiler.Rec<Compiler.recp<' (let [a "test is this"] (str "a b" a))'>> = ['(', 'let', '[', 'a', '"', 'test', 'is', 'this','"', ']', '(', 'str', '"', 'a', 'b', '"', 'a', ')', ')']
const recparsestrtest0: Compiler.Rec<Compiler.recp<` (let [a "test is this"] (str "a b" a))`>> = ['(', 'let', '[', 'a', '"test is this"', ']', '(', 'str', '"a b"', 'a', ')', ')']

// --- hash map ---
const recparsehashtest0: Compiler.Rec<Compiler.recp<' (let [a {:a 1 :b 2}] (> (:a a) (:b a)))'>> = ['(', 'let', '[', 'a', '{', ':a', '1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')']
const recparsehashtest1: Compiler.Rec<Compiler.recp<' (let [a {:a -1 :b 2}] (> (:a a) (:b a)))'>> = ['(', 'let', '[', 'a', '{', ':a', '-1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')']

// NOTE (A) : they spit a 2589 error with LegacyCompiler.SParser.
type crossX = '(fn [m0 m1] (>= (+ (:x m0) (:w m0)) (:x m1)))'
type m0 = '{:x 0 :w 1}'
type m1 = '{:x 1 :w 2}'
type aa = `(or (${crossX} ${m0} ${m1}) (:x {:x false}))`
type aaa = `(or (${crossX} ${m0} ${m1}) (${crossX} ${m1} ${m0}))`
type aaaa = ['(', 'or', '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '0', ':w', '1', '}', '{', ':x', '1', ':w', '2', '}', ')',  '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '1', ':w', '2', '}', '{', ':x', '0', ':w', '1', '}', ')', ')']
const aaaabb: aaaa = ['(', 'or', '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '0', ':w', '1', '}', '{', ':x', '1', ':w', '2', '}', ')',  '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '1', ':w', '2', '}', '{', ':x', '0', ':w', '1', '}', ')', ')']
type tesa = Compiler.Rec<Compiler.recp<Compiler.SPad<aaa>>>
const aaaatesa: tesa = ['(', 'or', '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '0', ':w', '1', '}', '{', ':x', '1', ':w', '2', '}', ')',  '(', '(', 'fn', '[', 'm0', 'm1', ']', '(', '>=', '(', '+', '(', ':x', 'm0', ')', '(', ':w', 'm0', ')', ')', '(', ':x', 'm1', ')', ')', ')', '{', ':x', '1', ':w', '2', '}', '{', ':x', '0', ':w', '1', '}', ')', ')']

const parseaaaaa: Compiler.SParser<' (x ((if a b c) y))'> =
    ['(', 'x', '(', '(', 'if', 'a', 'b', 'c', ')', 'y', ')', ')']
const parsebbbbb: Compiler.SParser<' (x (if a b c) y)'> =
    ['(', 'x', '(', 'if', 'a', 'b', 'c', ')', 'y', ')']
const parseccccc: Compiler.SParser<' ((f))'> =
    ['(', '(', 'f', ')', ')']
const parseddddd: Compiler.SParser<' ((((((x))))))'> =
    ['(', '(', '(', '(', '(', '(', 'x', ')', ')', ')', ')', ')', ')']
const parseeeeee: Compiler.SParser<' (let [a 1 b 2] (if true t f))'> = ['(','let', '[', 'a', '1', 'b', '2', ']', '(', 'if', 'true', 't', 'f', ')', ')']
// const parsestrtest0: Compiler.SParser<' (let [a "test is this"] (str "a b" a))'> = ['(', 'let', '[', 'a', '"', 'test', 'is', 'this','"', ']', '(', 'str', '"', 'a', 'b', '"', 'a', ')', ')']
const parsestrtest0: Compiler.SParser<' (let [a "test is this"] (str "a b" a))'> = ['(', 'let', '[', 'a', `"test is this"`, ']', '(', 'str', '"a b"', 'a', ')', ')']

// --- hash map ---
const parsehashtest0: Compiler.SParser<' (let [a {:a 1 :b 2}] (> (:a a) (:b a)))'> = ['(', 'let', '[', 'a', '{', ':a', '1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')']
const parsehashtest1: Compiler.SParser<' (let [a {:a -1 :b 2}] (> (:a a) (:b a)))'> = ['(', 'let', '[', 'a', '{', ':a', '-1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')']

const testsisnum0: Compiler.SIsNum<'-1102'> = true
const testsisnum1: Compiler.SIsNum<'-1102-'> = false
const testsisnum2: Compiler.SIsNum<'--1102'> = false
const testsisnum3: Compiler.SIsNum<'1102'> = true
const testsisnum4: Compiler.SIsNum<'001102'> = true
const testsisnum5: Compiler.SIsNum<'0011-0-2'> = false


const compileraaaa: Compiler.SCompiler<['(', '+', '0', '(', 'inc', '1', ')', ')']> = [['sym', '+'], ['prim', '0000000000000000'], [['sym', 'inc'], ['prim', '0000000000000001']]]
const compilerbbbb: Compiler.SCompiler<['(', 'let', '[', 'a', '1', ']', '(', 'if', 'true', 't', 'f', ')', ')']> = ['let', [['sym', 'a'], ['prim', '0000000000000001']], ['if', ['prim', true], ['sym', 't'], ['sym', 'f']]]
const compilercccc: Compiler.SCompiler<['(', '+', '-1', '2', ')']> = [['sym', '+'], ['prim', '1111111111111111'], ['prim', '0000000000000010']]

// -- String Parser
// const compilerStrT0: Compiler.SCompiler<['"', 'aaa', 'bbb','"']> = ['prim', '"aaa bbb"']
const compilerStrT1: Compiler.SCompiler<['(', 'let', '[', 'a', '"aaa bbb"',']', ')']> = ['let', [['sym', 'a'], ['prim', '"aaa bbb"']]]
// -- Hash map
const compilerHashT0: Compiler.SCompiler<['{', ':a', '01', ':b', '2', '}']> =
// 1
// ['map', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '10']]
['map', [['key', ':a'], ['prim', '0000000000000001'], ['key', ':b'], ['prim', '0000000000000010']]]
const compilerHashT1: Compiler.SCompiler<['{', ':a', '01', ':b', '2', ':c', '{', ':c1', '5', '}', '}']> =
// 1
// ['map', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '10'], ['key', ':c'], ['map', ['key', ':c1'], ['prim', '101']]]
['map', [['key', ':a'], ['prim', '0000000000000001'], ['key', ':b'], ['prim', '0000000000000010'], ['key', ':c'], ['map', [['key', ':c1'], ['prim', '0000000000000101']]]]]
const compilerHashT2: Compiler.SCompiler<['{', ':a', '01', ':b', '2', ':c', '{', ':c1', 'nil', '}', '}']> =
// 1
// ['map', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '10'], ['key', ':c'], ['map', ['key', ':c1'], ['prim', '101']]]
['map', [['key', ':a'], ['prim', '0000000000000001'], ['key', ':b'], ['prim', '0000000000000010'], ['key', ':c'], ['map', [['key', ':c1'], ['prim', 'nil']]]]]


const sencoderPrimT0: Compiler.SEncoder<['prim', 1]> = '1'
const sencoderPrimT1: Compiler.SEncoder<['prim', true]> = 'true'
const sencoderPrimT2: Compiler.SEncoder<['prim', 'string']> = "'string'"

const sencoderMapT0:  Compiler.SEncoder<['map', [['key', ':a'], ['prim', 1]]]> = '{:a 1}'
const sencoderMapT1:  Compiler.SEncoder<['map', [['key', ':a'], ['prim', 1], ['key', ':b'], ['prim', 2]]]> = '{:a 1 :b 2}'
const sencoderMapT2:  Compiler.SEncoder<['map', [['key', ':a'], ['prim', 1], ['key', ':b'], ['prim', 2], ['key', ':c'], ['map', [['key', ':ca'], ['prim', 3]]]]]> = '{:a 1 :b 2 :c {:ca 3}}'

const sencoderVecT0: Compiler.SEncoder<['vec', ['prim', 1], ['prim', 2], ['prim', 3]]> = '[1 2 3]'
const sencoderVecT1: Compiler.SEncoder<['vec', ['prim', 1], ['vec', ['prim', 2], ['prim', 3]]]> = '[1 [2 3]]'
const sencoderVecT2: Compiler.SEncoder<['vec']> = '[]'

const sencoderVecMapT0: Compiler.SEncoder<['vec', ['prim', 1], ['map', [['key', ':a'], ['prim', 2]]]]> = '[1 {:a 2}]'
const sencoderVecMapT1: Compiler.SEncoder<['map', [['key', ':a'], ['prim', 2], ['key', ':b'], ['vec', ['prim', 3], ['prim', 4]]]]> = '{:a 2 :b [3 4]}'

const sencoderIfT0: Compiler.SEncoder<['if', ['prim', true], ['prim', 1], ['prim', 2]]> = '(if true 1 2)'
const sencoderIfT1: Compiler.SEncoder<['if', ['prim', true], ['prim', 1]]> = '(if true 1)'

const sencoderLetT0: Compiler.SEncoder<[
  `let`,
  [[`sym`, `a`], [`prim`, `text-a`], [`sym`, `b`], [`prim`, `/text-b`]],
  [[`sym`, `str`], [`sym`, `a`], [`sym`, `b`]],
]> = "(let [a 'text-a' b '/text-b'] (str a b))"

const sencoderLetIfT0: Compiler.SEncoder<[
  `let`,
  [[`sym`, `a`], [`prim`, `text-a`], [`sym`, `b`], [`prim`, `/text-b`]],
  ['if', [['sym', '='], ['let', [['sym', 'aa'], ['prim', 1]], [['sym', '='], ['sym', 'aa'], ['prim', 1]]], ['prim', true]], [[`sym`, `str`], [`sym`, `a`], [`sym`, `b`]], ['prim', 1]],
]> = "(let [a 'text-a' b '/text-b'] (if (= (let [aa 1] (= aa 1)) true) (str a b) 1))"


const lisptest_str_0: Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"(str 'a' (str 's1' 's2'))">>> = [['sym', 'str'], ['prim', "'a'"], [['sym', 'str'], ['prim', "'s1'"], ['prim', "'s2'"]]]
const lisptest_str_1: Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"['a']">>> = ['vec', ['prim', "'a'"]]
const lisptest_plus_0: Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"(+ 01 (+ 10 11))">>> = [['sym', '+'], ['prim', '0000000000000001'], [['sym', '+'], ['prim', '0000000000001010'], ['prim', '0000000000001011']]]

// vector
const lisptest_vec_0: Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"[4 3]">>> = ['vec', ['prim', '0000000000000100'], ['prim', '0000000000000011']]
const lisptest_vec_1: Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"[4 3 [2 1]]">>> = ['vec', ['prim', '0000000000000100'], ['prim', '0000000000000011'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000001']]]
const lisptest_vec_2: Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"(let [a [4 3 [2 1]]] a)">>> = ['let', [['sym', 'a'], ['vec', ['prim', '0000000000000100'], ['prim', '0000000000000011'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000001']]]], ['sym', 'a']]

const lisptest_vec_3: Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"(first [])">>> = [['sym', 'first'], ['vec']]

const lisptest_let_0: Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"(let [x {:a 'a'} y 'a' z ['a']])">>> = ['let', [['sym', 'x'], ['map', [['key', ':a'], ['prim', "'a'"]]], ['sym', 'y'], ['prim', "'a'"], ['sym', 'z'], ['vec', ['prim', "'a'"]]]]



const unparsetest_prim_0: Compiler.Unparse<['prim', '0']> = '0'
const unparsetest_prim_1: Compiler.Unparse<['prim', "'str'"]> = "'str'"
const unparsetest_sym_0: Compiler.Unparse<['sym', 'x']> = 'x'

const unparsetest_fn_0: Compiler.Unparse<['fn', [['sym', 'x'], ['sym', 'y']], [['sym', '+'], ['sym', 'x'], ['sym', 'y']]]> = '(fn [x y] (+ x y))'
const unparsetest_fn_1: Compiler.Unparse<['fn', [['sym', 'x'], ['sym', 'y']], ['prim', '1']]> = '(fn [x y] 1)'
const unparsetest_fn_2: Compiler.Unparse<[['fn', [['sym', 'x'], ['sym', 'y']], [['sym', '+'], ['sym', 'x'], ['sym', 'y']]], ['prim', '2'], ['prim', '3']]> = '((fn [x y] (+ x y)) 2 3)'

const unparsetest_if_0: Compiler.Unparse<['if', ['prim', true], ['prim', '0']]> = '(if true 0)'
const unparsetest_if_1: Compiler.Unparse<['if', ['prim', true], ['prim', '0'], ['prim', '1']]> = '(if true 0 1)'
const unparsetest_if_2: Compiler.Unparse<['let', [['sym', 'a'], ['prim', '1']], ['if', ['prim', true], ['prim', '0'], ['prim', '1']]]> = '(let [a 1] (if true 0 1))'
const unparsetest_if_3: Compiler.Unparse<['let', [['sym', 'a'], ['prim', '1']], ['if', ['prim', true], ['prim', '0'], ['fn', [['sym', 'a'], ['sym', 'b']], ['prim' , '0']]]]> = '(let [a 1] (if true 0 (fn [a b] 0)))'

const unparsetest_let_0: Compiler.Unparse<['let', [['sym', 'a'], ['prim', '1']], ['prim', '1']]> = '(let [a 1] 1)'
const unparsetest_let_1: Compiler.Unparse<['let', [['sym', 'b'], ['prim', '10']], ['let', [['sym', 'a'], ['prim', '1']], ['prim', '1']]]> = '(let [b 2] (let [a 1] 1))'

const unparsetest_vec_0: Compiler.Unparse<['vec', ['prim', '0'], ['prim', '1']]> = '[0 1]'
const unparsetest_vec_1: Compiler.Unparse<['vec', ['key', ':a'], ['prim', '1']]> = '[:a 1]'
const unparsetest_vec_2: Compiler.Unparse<['vec', ['key', ':a'], ['prim', '1'], ['vec', ['prim', '01']]]> = '[:a 1 [1]]'
const unparsetest_vec_3: Compiler.Unparse<['vec']> = '[]'
const unparsetest_vec_4: Compiler.Unparse<['vec', ['prim', '0'], ['vec']]> = '[0 []]'
const unparsetest_vec_5: Compiler.Unparse<['vec', ['vec'], ['vec']]> = '[[] []]'

const unparsetest_map_0: Compiler.Unparse<['map', [['key', ':a'], ['prim', '0']]]> = '{:a 0}'
const unparsetest_map_1: Compiler.Unparse<['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1']]]> = '{:a 0 :b 1}'
const unparsetest_map_2: Compiler.Unparse<['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1'], ['key', ':c'], ['map', [['key', ':d'], ['prim', '10']]]]]> = '{:a 0 :b 1 :c {:d 2}}'

