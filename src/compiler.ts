import type Bit from './bit'
import type Decimal from './decimal'

// -------------------------------
// -- Compiler
// -------------------------------

namespace Compiler {

export type SPad<S extends string> = S extends ` ${infer SS}` ? SS  : ` ${S}`

export type SParser<Sexpr> =
  // -- ()
  Sexpr extends ` (${infer U}`
    ? ['(', ...SParser<` ${U}`>]
  : Sexpr extends ` ${infer V} ${infer W}`
    ? [...SParser<` ${V}`>, ...SParser<` ${W}`>]
  : Sexpr extends ` ${infer C})`
    ? [...SParser<` ${C}`>, ')']
  // -- []
  : Sexpr extends ` [${infer U}`
    ? ['[', ...SParser<` ${U}`>]
  : Sexpr extends ` ${infer V} ${infer W}`
    ? [...SParser<` ${V}`>, ...SParser<` ${W}`>]
  : Sexpr extends ` ${infer C}]`
    ? [...SParser<` ${C}`>, ']']
  // -- {}
  : Sexpr extends ` {${infer U}`
    ? ['{', ...SParser<` ${U}`>]
  : Sexpr extends ` ${infer V} ${infer W}`
    ? [...SParser<` ${V}`>, ...SParser<` ${W}`>]
  : Sexpr extends ` ${infer C}}`
    ? [...SParser<` ${C}`>, '}']
  // -- ""
  : Sexpr extends ` "${infer U}`
    ? [`"`, ...SParser<` ${U}`>]
  : Sexpr extends ` ${infer V} ${infer W}`
    ? [...SParser<` ${V}`>, ...SParser<` ${W}`>]
  : Sexpr extends ` ${infer C}"`
    ? [...SParser<` ${C}`>, `"`]
  // -- _, as default.
  : Sexpr extends ` ${infer CC}`
    ? [CC]
    : []

const parseaaaaa: SParser<' (x ((if a b c) y))'> =
    ['(', 'x', '(', '(', 'if', 'a', 'b', 'c', ')', 'y', ')', ')']
const parsebbbbb: SParser<' (x (if a b c) y)'> =
    ['(', 'x', '(', 'if', 'a', 'b', 'c', ')', 'y', ')']
const parseccccc: SParser<' ((f))'> =
    ['(', '(', 'f', ')', ')']
const parseddddd: SParser<' ((((((x))))))'> =
    ['(', '(', '(', '(', '(', '(', 'x', ')', ')', ')', ')', ')', ')']
const parseeeeee: SParser<' (let [a 1 b 2] (if true t f))'> = ['(','let', '[', 'a', '1', 'b', '2', ']', '(', 'if', 'true', 't', 'f', ')', ')']
const parsestrtest0: SParser<' (let [a "test is this"] (str "a b" a))'> = ['(', 'let', '[', 'a', '"', 'test', 'is', 'this','"', ']', '(', 'str', '"', 'a', 'b', '"', 'a', ')', ')']
// --- hash map ---
const parsehashtest0: SParser<' (let [a {:a 1 :b 2}] (> (:a a) (:b a)))'> = ['(', 'let', '[', 'a', '{', ':a', '1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')']
const parsehashtest1: SParser<' (let [a {:a -1 :b 2}] (> (:a a) (:b a)))'> = ['(', 'let', '[', 'a', '{', ':a', '-1', ':b', '2', '}', ']', '(', '>', '(', ':a', 'a', ')', '(', ':b', 'a', ')', ')', ')']
// --- string ---


type SIsNum<S, Top extends boolean = true> =
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

const testsisnum0: SIsNum<'-1102'> = true
const testsisnum1: SIsNum<'-1102-'> = false
const testsisnum2: SIsNum<'--1102'> = false
const testsisnum3: SIsNum<'1102'> = true
const testsisnum4: SIsNum<'001102'> = true
const testsisnum5: SIsNum<'0011-0-2'> = false

export type SSymlator<MSym> = 
  MSym extends `${infer H}${infer R}`
  ? H extends "'" | '"'
    ? [`prim`, MSym]
    : SIsNum<MSym> extends true
      ? H extends '-'
        ? ['prim', Bit.BitRevSign<Decimal.DecimalToBit<R>>]
        : ['prim', Decimal.DecimalToBit<MSym>]
    : MSym extends 'if' | 'let' | 'fn' // | ''
      ? MSym
      : MSym extends 'true'
        ? [`prim`, true]
          : MSym extends 'false'
            ? [`prim`, false]
            : [`sym`, MSym]
  : never

export type SCompiler<
    Parsed extends Array<unknown>,
    Current extends Array<unknown> = [],
    Stack extends Array<Array<unknown>> = [],
    StrStack extends string = "",
    IsLetVec extends boolean = false
    > = 
  Parsed extends []
  ? Current
  : Parsed extends [infer H, ...infer R]
    // -- terminate
    ? R extends []
      ? H extends '"'
        ? [`prim`, `${StrStack}"`]
        // -- Hash Case
        : H extends "}"
          ? ['map', Current] : Current
//        : Current
    // -- Not String Case
    : StrStack extends ""
      ? H extends ')' | ']'
        ? SCompiler<
          R
          , Stack extends unknown[] // the case of empty vector.
            ? [...Stack[0], Current extends ['vec', never] ? ['vec'] : Current]
            : never
          , Stack extends [infer _, ...infer R extends unknown[][]] ? R : never>
        // -- Hash Case Done.
        : H extends '}'
          ? SCompiler<R, Stack extends Array<unknown> ? [...Stack[0], ['map', Current]] : never, Stack extends [infer _, ...infer R extends unknown[][]] ? R : never>
        // -- Bracket Start
	  // -- List
        : H extends '('
          ? SCompiler<R, [], [Current, ...Stack]>
          // -- Vector
	: H extends '['
          ? SCompiler<R, IsLetVec extends true ? [] : ['vec'], [Current, ...Stack], StrStack>
          // -- Hash Case
        : H extends '{'
          ? SCompiler<R, [], [Current, ...Stack]>
          // -- Keyword Case
        : H extends `:${infer _}`
          ? SCompiler<R, [...Current, [`key`, H]], Stack>
          // -- Jump To String Case
          : H extends '"'
            ? SCompiler<R, Current, Stack, `${StrStack}${H}`>
	      // -- Symbol or Primitive case -- Default
	    : SCompiler<R, [...Current, SSymlator<H>], Stack, StrStack, H extends 'let' | 'fn' ? true : false>
      // -- String Case
      : H extends '"'
        ? SCompiler<R, [...Current, [`prim`, `${StrStack}"`]], Stack, "">
        : H extends string
          ? SCompiler<R, Current, Stack, StrStack extends '"' ? `${StrStack}${H}` : `${StrStack} ${H}`>
          : never
    : never
}

const compileraaaa: Compiler.SCompiler<['(', '+', '0', '(', 'inc', '1', ')', ')']> = [['sym', '+'], ['prim', '0000000000000000'], [['sym', 'inc'], ['prim', '0000000000000001']]]
const compilerbbbb: Compiler.SCompiler<['(', 'let', '[', 'a', '1', ']', '(', 'if', 'true', 't', 'f', ')', ')']> = ['let', [['sym', 'a'], ['prim', '0000000000000001']], ['if', ['prim', true], ['sym', 't'], ['sym', 'f']]]
const compilercccc: Compiler.SCompiler<['(', '+', '-1', '2', ')']> = [['sym', '+'], ['prim', '1111111111111111'], ['prim', '0000000000000010']]

// -- String Parser
const compilerStrT0: Compiler.SCompiler<['"', 'aaa', 'bbb','"']> = ['prim', '"aaa bbb"']
const compilerStrT1: Compiler.SCompiler<['(', 'let', '[', 'a', '"', 'aaa', 'bbb','"',']', ')']> = ['let', [['sym', 'a'], ['prim', '"aaa bbb"']]]
// -- Hash map
const compilerHashT0: Compiler.SCompiler<['{', ':a', '01', ':b', '2', '}']> =
// 1
// ['map', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '10']]
['map', [['key', ':a'], ['prim', '0000000000000001'], ['key', ':b'], ['prim', '0000000000000010']]]
const compilerHashT1: Compiler.SCompiler<['{', ':a', '01', ':b', '2', ':c', '{', ':c1', '5', '}', '}']> =
// 1
// ['map', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '10'], ['key', ':c'], ['map', ['key', ':c1'], ['prim', '101']]]
['map', [['key', ':a'], ['prim', '0000000000000001'], ['key', ':b'], ['prim', '0000000000000010'], ['key', ':c'], ['map', [['key', ':c1'], ['prim', '0000000000000101']]]]]



type CloseBracket<
  S extends string
, B extends string> =
  B extends 'map'
  ? `{${S}}`
  : B extends 'vec'
  ? `[${S}]`
  : B extends 'list'
  ? `(${S})`
  : S

type SEncoder<
  V extends unknown[]
// , Stack extends string[] = []
, Bracket extends 'map' | 'vec' | 'list' | 'unroll' = 'unroll'> =
  V extends ['sym', infer U extends string] // todo 
  ? `${U}`
  : V extends ['prim', infer U extends string | number | boolean]
  ? U extends string
    ? `'${U}'`
    : `${U}`
  : V extends ['map', infer U extends unknown[]]
    ? `{${SEncoder<U>}}`
    : V extends ['vec', ...infer U extends unknown[]]
    ? `[${SEncoder<U>}]`
    : V extends ['if'
		 , infer  B extends unknown[]
		 , infer TP extends unknown[]
		 , infer FP extends unknown[]]
    ? `(if ${SEncoder<B, 'list'>} ${SEncoder<TP, 'list'>} ${SEncoder<FP, 'list'>})`
    : V extends ['if'
		 , infer  B extends unknown[]
		 , infer TP extends unknown[]]
    ? `(if ${SEncoder<B, 'list'>} ${SEncoder<TP, 'list'>})`
    : V extends ['let'
		, infer U extends unknown[]
		, infer S extends unknown[]]
    ? `(let ${SEncoder<U, 'vec'>} ${SEncoder<S, 'list'>})`
    : V extends ['key', infer U extends string] // todo
      ? `${U}`
      : V extends [infer U extends unknown[], ...infer R extends unknown[][]]
        ? R extends []
          ? CloseBracket<`${SEncoder<U>}`, Bracket>
          : CloseBracket<`${SEncoder<U>} ${SEncoder<R>}`, Bracket>
        : ''

const sencoderPrimT0: SEncoder<['prim', 1]> = '1'
const sencoderPrimT1: SEncoder<['prim', true]> = 'true'
const sencoderPrimT2: SEncoder<['prim', 'string']> = "'string'"

const sencoderMapT0:  SEncoder<['map', [['key', ':a'], ['prim', 1]]]> = '{:a 1}'
const sencoderMapT1:  SEncoder<['map', [['key', ':a'], ['prim', 1], ['key', ':b'], ['prim', 2]]]> = '{:a 1 :b 2}'
const sencoderMapT2:  SEncoder<['map', [['key', ':a'], ['prim', 1], ['key', ':b'], ['prim', 2], ['key', ':c'], ['map', [['key', ':ca'], ['prim', 3]]]]]> = '{:a 1 :b 2 :c {:ca 3}}'

const sencoderVecT0: SEncoder<['vec', ['prim', 1], ['prim', 2], ['prim', 3]]> = '[1 2 3]'
const sencoderVecT1: SEncoder<['vec', ['prim', 1], ['vec', ['prim', 2], ['prim', 3]]]> = '[1 [2 3]]'
const sencoderVecT2: SEncoder<['vec']> = '[]'

const sencoderVecMapT0: SEncoder<['vec', ['prim', 1], ['map', [['key', ':a'], ['prim', 2]]]]> = '[1 {:a 2}]'
const sencoderVecMapT1: SEncoder<['map', [['key', ':a'], ['prim', 2], ['key', ':b'], ['vec', ['prim', 3], ['prim', 4]]]]> = '{:a 2 :b [3 4]}'

const sencoderIfT0: SEncoder<['if', ['prim', true], ['prim', 1], ['prim', 2]]> = '(if true 1 2)'
const sencoderIfT1: SEncoder<['if', ['prim', true], ['prim', 1]]> = '(if true 1)'

const sencoderLetT0: SEncoder<[
  `let`,
  [[`sym`, `a`], [`prim`, `text-a`], [`sym`, `b`], [`prim`, `/text-b`]],
  [[`sym`, `str`], [`sym`, `a`], [`sym`, `b`]],
]> = "(let [a 'text-a' b '/text-b'] (str a b))"

const sencoderLetIfT0: SEncoder<[
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



type Unparse<
  AST
, Type extends 'vec' | 'list' | 'map' | 'atom' = 'list'> = 
  AST extends infer H
    ? H extends ['prim' | 'sym' | 'key', infer r0 extends boolean | string]
      ? r0 extends string 
        ? Decimal.IsBitExpr<r0> extends true
          ? Decimal.BitToDecimal<r0>
        : `${r0}`
      : `${r0}`
    : H extends ['vec', ...infer r]
      ? r extends []
        ? '[]'
        : Unparse<r, 'vec'>
    : H extends ['map', infer r]
      ? Unparse<r, 'map'>

    : H extends ['fn', infer r0, infer r1]
      ? CloseBracket<`fn ${Unparse<r0, 'vec'>} ${Unparse<r1>}`, 'list'>
    : H extends ['let', infer r0, infer r1]
      ? CloseBracket<`let ${Unparse<r0, 'vec'>} ${Unparse<r1>}`, 'list'>
    : H extends ['if', infer r0, infer r1, ...infer r2]
      ? CloseBracket<`if ${Unparse<r0>} ${Unparse<r1>}${r2 extends [] ? '' : ' '}${Unparse<r2, 'atom'>}`, Type>
    : H extends [infer H extends unknown[], ...infer T]
      ? CloseBracket<`${Unparse<H>}${T extends [] ? '' : ' '}${Unparse<T, 'atom'>}`, Type>
    : H extends []
      ? ''
    : never
  : never

const unparsetest_prim_0: Unparse<['prim', '0']> = '0'
const unparsetest_prim_1: Unparse<['prim', "'str'"]> = "'str'"
const unparsetest_sym_0: Unparse<['sym', 'x']> = 'x'

const unparsetest_fn_0: Unparse<['fn', [['sym', 'x'], ['sym', 'y']], [['sym', '+'], ['sym', 'x'], ['sym', 'y']]]> = '(fn [x y] (+ x y))'
const unparsetest_fn_1: Unparse<['fn', [['sym', 'x'], ['sym', 'y']], ['prim', '1']]> = '(fn [x y] 1)'
const unparsetest_fn_2: Unparse<[['fn', [['sym', 'x'], ['sym', 'y']], [['sym', '+'], ['sym', 'x'], ['sym', 'y']]], ['prim', '2'], ['prim', '3']]> = '((fn [x y] (+ x y)) 2 3)'

const unparsetest_if_0: Unparse<['if', ['prim', true], ['prim', '0']]> = '(if true 0)'
const unparsetest_if_1: Unparse<['if', ['prim', true], ['prim', '0'], ['prim', '1']]> = '(if true 0 1)'
const unparsetest_if_2: Unparse<['let', [['sym', 'a'], ['prim', '1']], ['if', ['prim', true], ['prim', '0'], ['prim', '1']]]> = '(let [a 1] (if true 0 1))'
const unparsetest_if_3: Unparse<['let', [['sym', 'a'], ['prim', '1']], ['if', ['prim', true], ['prim', '0'], ['fn', [['sym', 'a'], ['sym', 'b']], ['prim' , '0']]]]> = '(let [a 1] (if true 0 (fn [a b] 0)))'

const unparsetest_let_0: Unparse<['let', [['sym', 'a'], ['prim', '1']], ['prim', '1']]> = '(let [a 1] 1)'
const unparsetest_let_1: Unparse<['let', [['sym', 'b'], ['prim', '10']], ['let', [['sym', 'a'], ['prim', '1']], ['prim', '1']]]> = '(let [b 2] (let [a 1] 1))'

const unparsetest_vec_0: Unparse<['vec', ['prim', '0'], ['prim', '1']]> = '[0 1]'
const unparsetest_vec_1: Unparse<['vec', ['key', ':a'], ['prim', '1']]> = '[:a 1]'
const unparsetest_vec_2: Unparse<['vec', ['key', ':a'], ['prim', '1'], ['vec', ['prim', '01']]]> = '[:a 1 [1]]'
const unparsetest_vec_3: Unparse<['vec']> = '[]'
const unparsetest_vec_4: Unparse<['vec', ['prim', '0'], ['vec']]> = '[0 []]'
const unparsetest_vec_5: Unparse<['vec', ['vec'], ['vec']]> = '[[] []]'

const unparsetest_map_0: Unparse<['map', [['key', ':a'], ['prim', '0']]]> = '{:a 0}'
const unparsetest_map_1: Unparse<['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1']]]> = '{:a 0 :b 1}'
const unparsetest_map_2: Unparse<['map', [['key', ':a'], ['prim', '0'], ['key', ':b'], ['prim', '1'], ['key', ':c'], ['map', [['key', ':d'], ['prim', '10']]]]]> = '{:a 0 :b 1 :c {:d 2}}'


export default Compiler
