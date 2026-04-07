import type * as Bit from './bit.js'
import type * as Decimal from './decimal.js'
import type { StrUtil as str } from './strutil.js'
import type { VecUtil as vec } from './vecutil.js'
import type { regexConst } from './regex-const.js'
import type { Util as u } from './util.js'

export type CompTape = CompFrame | CompFrame[] | CompEnv
export type CompState = string
export type CompFrame = string | CompTape[] | CompFrame[] | FrameCharaClass
export type FrameCharaClass = ['chara-class', string[]]
export type CompEnv = {state: CompState, tapes: CompTape[], frames: CompFrame[]}
export type InitCompEnv = {state: '', tapes: [], frames: []}

export type CompSig = '?' | '*' | 'group' | 'or-group' | 'chara-class'

type InitStack = []
type InitLast = ''
type InitCondition = ''

export type ReadInter<
  S extends string> =
  S extends `${infer f extends keyof regexConst.TransNumber}-${infer s extends keyof regexConst.TransNumber}${infer _rest}`
    ? vec.BitInter<regexConst.ASCII, regexConst.TransNumber[f], regexConst.TransNumber[s]>
  : never

type NegCharaClass<
  V extends (keyof regexConst.TransNumber)[]> =
  u.DissocKeys<regexConst.TransNumber, V> extends infer KU extends Record<PropertyKey, unknown>
    ? u.KeysTuple<KU> extends infer R
      ? R
    : never
  : never

type ReturnCC<
  Sig extends '[' | '[^'
, R extends (keyof regexConst.TransNumber)[]> =
  Sig extends '['
    ? R
  : NegCharaClass<R>

type CompOption = {negExpand: boolean}
type InitCompOption = {negExpand: true}
export type CompCharaClass<
  S extends string
, Sig extends '[' | '[^'
, Option extends CompOption = InitCompOption> =
_CompCharaClass<S, Sig, Option>

type _CompCharaClass<
  S extends string
, Sig extends '[' | '[^'
, Option extends CompOption = InitCompOption
, R extends (keyof regexConst.TransNumber)[] = []
, IsReading extends boolean = false> =
  S extends ''
    ? [['chara-class', true extends (Option['negExpand']) ? ReturnCC<Sig, R> : R], '']
  : str.RegCut<S> extends [infer regFirst extends string
                          , infer regRest extends string]
    ? regFirst extends ']'
      ? [['chara-class', true extends (Option['negExpand']) ? ReturnCC<Sig, R> : R], regRest]
    : regFirst extends '[' | '[^'
      ? IsReading extends false
        ? _CompCharaClass<regRest, Sig, Option, R, true>
      : _CompCharaClass<regRest, Sig, Option, [...R, ...(regFirst extends '[' ? ['['] : ['[', '^'])], true>
    : regFirst extends `\\${infer Escaped}`
      ? regFirst extends keyof regexConst.MetaChars
        ? _CompCharaClass<regRest, Sig, Option, [...R, ...regexConst.MetaChars[regFirst]], IsReading>
      : Escaped extends keyof regexConst.TransNumber
        ? _CompCharaClass<regRest, Sig, Option, [...R, Escaped], IsReading>
      : 'never6'
    : regFirst extends keyof regexConst.MetaChars
      ? _CompCharaClass<regRest, Sig, Option, [...R, ...regexConst.MetaChars[regFirst]], IsReading>
    : regFirst extends keyof regexConst.TransNumber
      ? _CompCharaClass<regRest, Sig, Option, [...R, regFirst], IsReading>
    : regFirst extends `${infer _fst}-${infer _trd}${infer _rest}`
      ? ReadInter<regFirst> extends infer Range extends (keyof regexConst.TransNumber)[]
        ? _CompCharaClass<regRest, Sig, Option, [...R, ...Range], IsReading>
      : never
    : never
  : never

export type UnrollRepeat<
  Times extends string
, P> =
  Times extends "0000000000000010"
    ? [P, P]
  : never

export type ExpandMinMax<
  P extends CompFrame
, min extends string
, max extends string> =
  max extends '='
    ? vec.BitRepeat<min,P> extends infer r
      ? r extends CompFrame
        ? r
      : never
    : never
  : max extends '<'
    ? vec.BitRepeat<min,P> extends infer _Drp extends unknown[]
      ? [...(_Drp), ['*', P]] extends infer r
        ? r extends CompFrame
          ? r
        : never
      : never
    : never
  : vec.BitRepeat<min,P> extends infer _Dfst extends unknown[]
    ? Bit.BitSub<max,min> extends infer _Dsub extends string
      ? vec.BitRepeat<_Dsub, ['?', P]> extends infer _Dsnd extends unknown[]
        ? [..._Dfst, ..._Dsnd] extends infer r
          ? r extends CompFrame
            ? r
          : never
        : never
      : never
    : never
  : never

// export type CompMinMax<
//   S extends string> =
//   S extends `{${infer pax}${infer pay},}${infer Next}`
//     ? [Decimal.DtoB<`${pax}${pay}`>,'<', Next]
//   : S extends `{${infer pa},}${infer Next}`
//     ? [Decimal.DtoB<pa>,'<', Next]
//   : S extends `{${infer n},${infer m}}${infer Next}`
//     ? [Decimal.DtoB<n>,Decimal.DtoB<m>, Next]
//   : S extends `{${infer na},${infer ma}${infer mb}}${infer Next}`
//     ? [Decimal.DtoB<na>,Decimal.DtoB<`${ma}${mb}`>, Next]
//   : S extends `{${infer nna}${infer nnb},${infer mma}${infer mmb}}${infer Next}`
//     ? [Decimal.DtoB<`${nna}${nnb}`>, Decimal.DtoB<`${mma}${mmb}`>, Next]
//   : S extends `{${infer zz}}${infer Next}`
//     ? [Decimal.DtoB<zz>,'=', Next]
//   : S extends `{${infer zza}${infer zzb}}${infer Next}`
//     ? [Decimal.DtoB<`${zza}${zzb}`>,'=', Next]
//   : []

export type CompMinMax<
  S extends string> =
  S extends `${infer s0}${infer s1}${infer s2}${infer Next}`
    ? [s0, s2] extends ['{', '}']
      ? [Decimal.DtoB<s1>, '=', Next]
    : [s0, s2] extends ['{', ',']
      ? Next extends `${infer s3}${infer nNext}`
        ? s3 extends '}'
          ? [Decimal.DtoB<s1>, '<', nNext]
        : nNext extends `${infer s4}${infer nnNext}`
          ? s4 extends '}'
            ? [Decimal.DtoB<s1>, Decimal.DtoB<s3>, nnNext]
          : nnNext extends `${infer s5}${infer FinNext}`
            ? s5 extends '}'
              ? [Decimal.DtoB<s1>, Decimal.DtoB<`${s3}${s4}`>, FinNext]
            : []
          : []
        : []
      : []
    : Next extends `${infer s3}${infer nNext}`
      ? s3 extends '}'
        ? [Decimal.DtoB<`${s1}${s2}`>, '=', nNext]
      : s3 extends ','
        ? nNext extends `${infer s4}${infer nnNext}`
          ? s4 extends '}'
            ? [Decimal.DtoB<`${s1}${s2}`>, '<', nnNext]
          : nnNext extends `${infer s5}${infer s6}${infer finNext}`
            ? s6 extends '}'
              ? [Decimal.DtoB<`${s1}${s2}`>, Decimal.DtoB<`${s4}${s5}`>, finNext]
            : []
          : []
        : []
      : []
    : []
  : []

type FrameToTape<
  F extends CompEnv> =
  F extends {state: infer State extends CompState, tapes: infer Tapes extends CompTape[], frames: infer Frames extends CompFrame[]}
    ? { state: State
      , tapes: [...Tapes, ...Frames]
      , frames: [] }
  : never

// type CompTaping<F extends CompEnv, Fr extends CompFrame> = 
//   F extends {state: infer State extends CompState, tapes: infer Tapes extends CompTape[], frames: infer Frames extends CompFrame[]}
//    ? {state: State, tapes: [...Tapes, Fr], frames: Frames}
//    : never

type CompFraming<
  F extends CompEnv
, Fr extends CompFrame> =
  F extends {state: infer State extends CompState, tapes: infer Tapes extends CompTape[], frames: infer Frames extends CompFrame[]}
    ? { state: State
      , tapes: Tapes
      , frames: [...Frames, Fr] }
  : never

// -----------------------------------------------------
// [Note]
//
// `Last` delays the transfer of `env['frames']` to `env['tapes']`
// because evaluating `?`, `*`, and `+` requires the previous frame
// to wrap it.
//
// Therefore, all frames are transferred to tapes via `Last`.
// A portion of the regex is compiled into `Last`, and the previous `Last`
// is also packed into `frames` at the same time.
//
// `frames` are packed into `tapes` when the `state` switches.
// It happens if `Comp` reads a start of group or a similar construct.
// -----------------------------------------------------
// 
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
//
// ------------------------------------------------------
export type recComp<
  S extends string
, Option extends CompOption = InitCompOption
, Stack extends CompEnv[] = InitStack
, env extends CompEnv = InitCompEnv
, Last extends CompFrame = InitLast
, condition extends string = InitCondition> =
  S extends ''
    ? Last extends ''
      ? { r: { condition: condition
             , tapes: [...env['tapes'], ...env['frames']] } } extends infer r
        ? r
      : never
    : { r: { condition: condition
           , tapes: [...env['tapes'], ...[...env['frames'], Last]] } } extends infer r
      ? r
    : never
  : env extends { state:  infer State extends CompState
                , tapes:  infer Tapes extends CompTape[]
		, frames: infer Frames extends CompFrame[]}
    ? str.RegCut<S> extends [ infer regFirst extends string
                            , infer regRest extends string]
      ? regFirst extends '^' | '$'
        ? { r: recComp<regRest, Option, Stack, env, Last, `${condition}${regFirst}`> }
      : regFirst extends '[' | '[^'
        ? CompCharaClass<S, regFirst, Option> extends [infer classComped extends FrameCharaClass, infer classRest extends string]
          ? FrameToTape<CompFraming<env, Last>> extends infer FramedEnv extends CompEnv
            ? {r: recComp<classRest, Option, Stack, Last extends '' ? env : FramedEnv, classComped, condition>} extends infer a
              ? a
            : never
          : never
        : never
      : regFirst extends '('
        ? { r: recComp<regRest, Option, [Last extends InitLast ? env : CompFraming<env, Last>, ...Stack], {state: 'group', tapes: [], frames: []}, InitLast, condition> }
      : regFirst extends ')'
        ? Stack extends [ infer stackFirst extends CompEnv
                        , ...infer stackRest extends CompEnv[]]
          ? { r: recComp<regRest, Option, stackRest, stackFirst, [State, ...Tapes, ...(Last extends InitLast ? Frames : [[...Frames, Last]])], condition> }
        : never
      : regFirst extends '|'
        ? { r: recComp<regRest, Option, Stack, {state: 'or-group', tapes: [...Tapes, Last extends InitLast ? Frames : [...Frames, Last]], frames: []}, '', condition> }
      : regFirst extends '?' | '*'
        ? { r: recComp<regRest, Option, Stack, CompFraming<FrameToTape<env>, [regFirst, Last]>, InitLast, condition> }
      : regFirst extends '+'
        ? { r: recComp<regRest, Option, Stack, {state: State, tapes:[ ...Tapes, ...Frames, Last, ['*', Last]], frames: []}, InitLast, condition> }
      : regFirst extends '{'
        ? CompMinMax<S> extends [infer Min extends string,
                                 infer Max extends string
                                , infer Next extends string]
          ? ExpandMinMax<Last, Min, Max> extends infer _Delay extends CompFrame
            ? { r: recComp<Next, Option, Stack, env, _Delay, condition> extends infer r?r:never }
          : never
        : never
      : { r: recComp<regRest, Option, Stack, Last extends "" ? env : CompFraming<env, Last>, regFirst, condition> }
    : never
  : never

export type Comp<
  S extends string
, Option extends CompOption = InitCompOption> =
u.Rec<recComp<S, u.AssocWith<InitCompOption, Option>>>

export type * as regexCompiler from './regex-compiler.js'
