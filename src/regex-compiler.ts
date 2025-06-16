import type * as Bit from './bit'
import type * as Decimal from './decimal'
import type { StrUtil as str } from './strutil'
import type { VecUtil as vec } from './vecutil'
import type { regexConst } from './regex-const'
import type { Util as u } from './util'

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
    ? vec.Inter<regexConst.ASCII, regexConst.TransNumber[f], regexConst.TransNumber[s]>
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

export type CompCharaClass<S extends string, Sig extends '[' | '[^'> = _CompCharaClass<S, Sig>

type _CompCharaClass<
  S extends string
, Sig extends '[' | '[^'
, R extends (keyof regexConst.TransNumber)[] = []> =
  S extends ''
    ? [['chara-class', ReturnCC<Sig, R>], '']
  : str.RegCut<S> extends [infer regFirst extends string
                          , infer regRest extends string]
    ? regFirst extends ']'
      ? [['chara-class', ReturnCC<Sig, R>], regRest]
    : regFirst extends '[' | '[^'
      ? _CompCharaClass<regRest, Sig, R>
    : regFirst extends `${infer _fst}-${infer _trd}`
      ? ReadInter<regFirst> extends infer Range extends (keyof regexConst.TransNumber)[]
        ? _CompCharaClass<regRest, Sig, [...R, ...Range]>
      : never
    : regFirst extends keyof regexConst.MetaChars
      ? _CompCharaClass<regRest, Sig, [...R, ...regexConst.MetaChars[regFirst]]>
    : regFirst extends keyof regexConst.TransNumber
      ? _CompCharaClass<regRest, Sig, [...R, regFirst]>
    : never
  : never

export type ExpandMinMax<
  P extends CompFrame
, min extends string
, max extends string> =
  max extends '='
    ? vec.Repeat<min,P>
  : max extends '<'
    ? [...vec.Repeat<min,P>, ['*', P]]
  : [...vec.Repeat<min,P>, ...vec.Repeat<Bit.BitSub<max,min>, ['?', P]>]

type CompMinMax<
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
, Stack extends CompEnv[] = InitStack
, env extends CompEnv = InitCompEnv
, Last extends CompFrame = InitLast
, condition extends string = InitCondition> =
  S extends ''
    ? Last extends ''
      ? { r: { condition: condition
        , tapes: [...env['tapes'], ...env['frames']] } }
    : { r: { condition: condition
           , tapes: [...env['tapes'], ...[...env['frames'], Last]] } }
  : env extends { state:  infer State extends CompState
                , tapes:  infer Tapes extends CompTape[]
		, frames: infer Frames extends CompFrame[]}
    ? str.RegCut<S> extends [ infer regFirst extends string
                            , infer regRest extends string]
      ? regFirst extends '^' | '$'
        ? { r: recComp<regRest, Stack, env, Last, `${condition}${regFirst}`> }
      : regFirst extends '[' | '[^'
        ? CompCharaClass<S, regFirst> extends [infer classComped extends FrameCharaClass, infer classRest extends string]
          ? FrameToTape<CompFraming<env, Last>> extends infer FramedEnv extends CompEnv
            ? {r: recComp<classRest, Stack, Last extends '' ? env : FramedEnv, classComped, condition>} extends infer a
              ? a
            : never
          : never
        : never
      : regFirst extends '('
        ? { r: recComp<regRest, [Last extends "" ? env : CompFraming<env, Last>, ...Stack], {state: 'group', tapes: [], frames: []}, InitLast, condition> }
      : regFirst extends ')'
        ? Stack extends [ infer stackFirst extends CompEnv
                        , ...infer stackRest extends CompEnv[]]
          ? { r: recComp<regRest, stackRest, stackFirst, [State, ...Tapes, (Last extends "" ? Frames : [...Frames, Last])], condition> }
        : never
      : regFirst extends '|'
        ? { r: recComp<regRest, Stack, {state: 'or-group', tapes: [...Tapes, Last extends "" ? Frames : [...Frames, Last]], frames: []}, '', condition> }
      : regFirst extends '?' | '*'
        ? { r: recComp<regRest, Stack, CompFraming<FrameToTape<env>, [regFirst, Last]>, InitLast, condition> }
      : regFirst extends '+'
        ? { r: recComp<regRest, Stack, {state: State, tapes:[ ...Tapes, ...Frames, Last, ['*', Last]], frames: []}, InitLast, condition> }
      : regFirst extends '{'
        ? CompMinMax<S> extends [infer Min extends string,
                                 infer Max extends string
                                , infer Next extends string]
          ? ExpandMinMax<Last, Min, Max> extends infer _Delay extends CompFrame
            ? { r: recComp<Next, Stack, env, _Delay, condition> }
          : never
        : never
      : { r: recComp<regRest, Stack, Last extends "" ? env : CompFraming<env, Last>, regFirst, condition> }
    : never
  : never

export type Comp<S extends string> = u.Rec<recComp<S>>

export type * as regexCompiler from './regex-compiler'
