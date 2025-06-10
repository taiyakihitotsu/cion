import type * as Bit from './bit'
import type * as Decimal from './decimal'
import type { StrUtil as str } from './strutil'
import type { VecUtil as vec } from './vecutil'
import type { tZero, tOne, tTwo } from './strutil'
import { vZero, vOne, vTwo } from './strutil'
import type { regexConst } from './regex-const'


// -------------------------
// -- Regex Compiler
// -------------------------

export type ExpandMinMax<
  P extends string[]
, min extends string
, max extends string> =
[...vec.Repeat<min, P>, ...(max extends '=' ? [] : max extends '<' ? [[P, '*']] : vec.Repeat<Bit.BitSub<max,min>, [P, '?']>)]

export type CompMinMax<
  S extends string> =
  S extends `{${infer pa},}${infer Next}`
    ? [Decimal.DtoB<pa>,'<', Next]
  : S extends `{${infer pax}${infer pay},}${infer Next}`
    ? [Decimal.DtoB<`${pax}${pay}`>,'<', Next]
  : S extends `{${infer n},${infer m}}${infer Next}`
    ? [Decimal.DtoB<n>,Decimal.DtoB<m>, Next]
  : S extends `{${infer na},${infer ma}${infer mb}}${infer Next}`
    ? [Decimal.DtoB<na>,Decimal.DtoB<`${ma}${mb}`>, Next]
  : S extends `{${infer nna}${infer nnb},${infer mma}${infer mmb}}${infer Next}`
    ? [Decimal.DtoB<`${nna}${nnb}`>, Decimal.DtoB<`${mma}${mmb}`>, Next]
  : S extends `{${infer zz}}${infer Next}`
    ? [Decimal.DtoB<zz>,'=', Next]
  : S extends `{${infer zza}${infer zzb}}${infer Next}`
    ? [Decimal.DtoB<`${zza}${zzb}`>,'=', Next]
  : []

type ReadEscape<
  S extends string> =
  S extends `\\${infer Escaped}${infer Rest}`
    ? Escaped extends str.MetaChars
      ? [`\\${Escaped}`, Rest]
    : [`${Escaped}`, Rest]
  : never

type CompFailed = []

export type ReadInter<
  S extends string> =
  S extends `${infer f extends keyof regexConst.TransNumber}-${infer s extends keyof regexConst.TransNumber}${infer rest}`
    ? [vec.Inter<regexConst.CharList, regexConst.TransNumber[f], regexConst.TransNumber[s]>, rest]
  : never

// Compiler
//
// This function compiles a regex string into an array, which we call a `tape`.
// The final result is a record containing both conditions and the tape.
//
// Here is how each element of the tape is translated from each part of the regex:
//
// - A single character like `c` is compiled into ['c'], a one-element array.
// - A group like `(abc)` is compiled into ['abc'].
// - A union group like `(a|b)` is compiled into ['a', 'b'].
//   These are passed into a `OrMatch`, where each element is read during matching.
// - A character set like `[ab]` is treated the same as a union group: ['a', 'b'].
//
// When `*` or `+` appears, the preceding element is packed into a tuple:
//   - The first item is the compiled array (e.g., ['abc', 'de'])
//   - The second item is the operator string: `*`.
//
//   For example, `(abc|de)*` first becomes ['abc', 'de'], then `*` is read,
//   so it becomes [['abc', 'de'], '*'] (example A).
//
//   If it's a `+`, another ['abc', 'de'] is appended the tape, before the tuple A is added to it.
//   So the tape ends with: [...tape, ['abc', 'de'], ['abc', 'de'], '*'].
//
// `{n,m}` is compiled in the same way as `+`.
// - `(pat){2}` is expanded to `[['pat'], ['pat']]`
// - `(pat){2,}` is expanded to `[['pat'], ['pat'], [['pat'], '*']]`
// - `(pat){2,4}` is expanded to `[['pat'], ['pat'], [['pat'], '?'], [['pat'], '?']]`
//
// `^` and `$` are compiled into condition flags.
type R<V, Type> = V extends Type ? V : never
type RegF = [string[], ('*'|'?')]

type _Comp<
  S extends string
, IsMerge extends string
, MergeString extends string
, MergeStack extends string[] | RegF
, Stack extends unknown[]
, tobeStack extends string[] | RegF
, condition extends string> =
  S extends ''
    ? { condition: condition
      , tape: [...Stack, tobeStack] }
  : S extends `${infer sFirst}${infer sRest}`
    ? sFirst extends '^' | '$'
      ? _Comp<sRest, IsMerge, MergeString, MergeStack, Stack, tobeStack, `${condition}${sFirst}`>
    : sFirst extends '('
      ? _Comp<sRest, '(', '', MergeStack, [...Stack, tobeStack], [], condition>
    : sFirst extends ')'
      ? _Comp<sRest, '', '', [...R<MergeStack, string[]>, MergeString], [...Stack], [], condition>
    : sFirst extends '['
      ? _Comp<sRest, '[', '', MergeStack, [...Stack, tobeStack], [], condition>
    : sFirst extends ']'
      ? _Comp<sRest, '', '', [...R<MergeStack, string[]>, ...(MergeString extends '' ? [] : [MergeString])], [...Stack], [], condition>
    : sFirst extends '+'
      ? _Comp<sRest, '', '', [...(MergeStack extends [] ? [] : [R<MergeStack, string[]>, '*'])], [...Stack, ...(MergeStack extends [] ? [] : [MergeStack]), ...(tobeStack extends [] ? [] : [tobeStack])], [...(tobeStack extends [] ? [] : [R<tobeStack, string[]>, '*'])], condition>
    : sFirst extends '*'
      ? _Comp<sRest, '', '', [...(MergeStack extends [] ? [] : [R<MergeStack, string[]>, sFirst])], Stack, [...(tobeStack extends [] ? [] : [R<tobeStack, string[]>, sFirst])], condition>
    : sFirst extends '?'
      ? _Comp<sRest, '', '', [...(MergeStack extends [] ? [] : [R<MergeStack, string[]>, '?'])], Stack, [...(tobeStack extends [] ? [] : [R<tobeStack, string[]>, '?'])], condition>
    : sFirst extends '{'
      ? CompMinMax<S> extends [infer min extends string, infer max extends string, infer Next extends string]
        ? _Comp<Next, '', '', [], [...Stack, ...ExpandMinMax<(MergeStack extends [] ? R<tobeStack, string[]> : R<MergeStack, string[]>), min, max>], [], condition>
      : CompFailed
    : IsMerge extends '('
      ? sFirst extends '|'
        ? _Comp<sRest, '(', '', [...R<MergeStack, string[]>, MergeString], Stack, [], condition>
      : str.RegCut<S> extends [infer rFirst extends string, infer rSecond extends string]
        ? _Comp<rSecond, '(', `${MergeString}${rFirst}`, MergeStack, Stack, [], condition>
      : never
    : IsMerge extends '['
      ? S extends `${infer _a}-${infer _b}${infer _c}`
        ? ReadInter<S> extends [infer a extends string[], infer b extends string]
          ? _Comp<b, '[', '', [...R<MergeStack, string[]>, ...(MergeString extends '' ? [] : R<[MergeString], string[]>), ...a], Stack, [], condition>
        : 'test'
      : _Comp<str.RegFirstSplit<S,1>, '[', str.RegFirstSplit<S,0>, [...R<MergeStack, string[]>, ...(MergeString extends '' ? [] : [MergeString])], Stack, [], condition>
    : IsMerge extends ''
      ? sFirst extends '\\'
        ? ReadEscape<S> extends [infer sFirst extends string, infer srestRest extends string]
          ? _Comp<srestRest, '', '', [], [...Stack, ...(tobeStack extends [] ? [] : [tobeStack]), ...(MergeStack extends [] ? [] : [MergeStack])], [sFirst], condition>
        : never
      : _Comp<sRest, '', '', [], [...Stack, ...(tobeStack extends [] ? [] : [tobeStack]), ...(MergeStack extends [] ? [] : [MergeStack])], [sFirst], condition>
    : CompFailed
  : CompFailed

export type Comp<Regex extends string> = _Comp<Regex, '', '', [], [], [], ''>

// ------------------------
// -- [main] eval regexp
// ------------------------

export type OrMatch<
  S extends string
, Pat extends string[]
, Forward extends string = ''> =
  Pat extends []
    ? []
  : Pat extends [infer First extends string, ...infer Rest extends string[]]
    ? str.StrSearchAll<S, First, '^', Forward> extends [infer M extends string, infer Re extends string]
      ? [M, Re]
    : OrMatch<S, Rest, Forward>
  : []

type JustSymbolScene  = [string]
type UnionSymbolScene = string[]
type FnScene = [string[], ('*'|'?')]
type Scene = (JustSymbolScene|UnionSymbolScene|FnScene)
type TapeType = Scene[] 
type FnSigns = '*' | '?'

// TapeEval
//
// This function evaluates a compiled regex tape against an input string.
//
// [Note]
// This does not represent the full process of regex string matching.
// If this function is called only once, it behaves like matching a regex with `^`.
//
// [Note]
// The `+` operator is internally expanded into `*` during compilation.
//
// [Note]
//   Patterns like `ss*s` should ideally be merged into `ss*`,
//   just as `(xyz)*xyz` could be reduced to `(xyz)*`.
//   However, this optimization was abandoned to preserve greedy behavior,
//   so such patterns are considered illegal in this implementation.
export type TapeEval<
  String extends string
, Tape extends TapeType = []
, Forward extends string = ''> =
  Tape extends []
    ? [Forward, String]
  : Tape extends [infer firstTape extends Scene, ...infer restTape extends TapeType]
    ? firstTape extends infer Pats extends string[]
      ? OrMatch<String, Pats, Forward> extends [infer Matched extends string, infer Next extends string]
        ? TapeEval<Next, restTape, Matched>
      : []
    : firstTape extends [infer groupTape extends string[], infer Fn extends FnSigns]
      ? OrMatch<String, groupTape, Forward> extends [infer Matched extends string, infer Next extends string]
        ? TapeEval<Next, Fn extends '*' ? Tape : restTape, Matched>
      : TapeEval<String, restTape, Forward>
    : never
  : never

export type ReadTape<
  String extends string
, TapeEnv extends {condition: string, tape: TapeType}> =
  TapeEnv extends {condition: infer cond, tape: infer tape extends TapeType}
    ? TapeEval<String, tape>
  : []

export type TapeEvalLoop<
  S extends string
, Tape extends TapeType> =
  S extends ''
    ? []
  : TapeEval<S, Tape> extends infer R
    ? R extends []
      ? S extends `${infer _f}${infer sRest}`
        ? TapeEvalLoop<sRest, Tape>
      : never
    : R
  : never

export type RegexFind<
  String extends string
, Regex  extends string> =
  Comp<Regex> extends {condition: infer Condition extends string
		      , tape: infer Tape extends TapeType}
    ? Condition extends '^' | '^$'
      ? ReadTape<String, {condition: Condition, tape: Tape}>
    : Condition extends '$' | '^$'
      ? TapeEvalLoop<String, Tape> extends [infer M extends string, infer Result extends string]
        ? Result extends ''
          ? [M, Result]
        : []
      : []
    : TapeEvalLoop<String, Tape>
  : never

export type * as regex from './regex'
