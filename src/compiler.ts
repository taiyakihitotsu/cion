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
    ? ['"', ...SParser<` ${U}`>]
  : Sexpr extends ` ${infer V} ${infer W}`
    ? [...SParser<` ${V}`>, ...SParser<` ${W}`>]
  : Sexpr extends ` ${infer C}"`
    ? [...SParser<` ${C}`>, '"']
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
      ? H extends ')' | ']' // | '}'
        ? SCompiler<R, Stack extends Array<unknown> ? [...Stack[0], Current] : never, Stack extends [infer _, ...infer R extends unknown[][]] ? R : never>
        // -- Hash Case Done.
        : H extends '}'
          ? SCompiler<R, Stack extends Array<unknown> ? [...Stack[0], ['map', Current]] : never, Stack extends [infer _, ...infer R extends unknown[][]] ? R : never>
        // -- Bracket Start
        : H extends '(' | '['
          ? SCompiler<R, [], [Current, ...Stack]>
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
            : SCompiler<R, [...Current, SSymlator<H>], Stack>
      // -- String Case
      : H extends '"'
        ? SCompiler<R, [...Current, [`prim`, `${StrStack}"`]], Stack, "">
        : H extends string
          ? StrStack extends '"'
            ? SCompiler<R, Current, Stack, `${StrStack}${H}`>
            : SCompiler<R, Current, Stack, `${StrStack} ${H}`>
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
const lisptest_plus_0: Compiler.SCompiler<Compiler.SParser<Compiler.SPad<"(+ 01 (+ 10 11))">>> = [['sym', '+'], ['prim', '0000000000000001'], [['sym', '+'], ['prim', '0000000000001010'], ['prim', '0000000000001011']]]

export default Compiler

