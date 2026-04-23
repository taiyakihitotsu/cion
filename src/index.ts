import type * as Bit from './bit/index.js'
import type * as Compiler from './compiler/index.js'
import type * as Util from './util.js'
import type * as Decimal from './decimal/index.js'
import type { regex } from './regex/index.js'
import type * as ratio from './ratio/index.js'
import type * as str from './strutil.js'
import type * as Vec from './vector/index.js'

import type {LetVal,LetArg,LetForm,Each,Atom,TMap,Sexpr,TNil,Keyword,Sym,PrimString,PrimBoolean,PrimTestNumber,PrimNumber,BitString,RatioString,NumString,Prim,Args,Fn,IFn,Vector,Var,Env,TNotMatch,IfForm, Falsy, NilLiteral, VecEmpty} from './sexprtypes.js'


import type * as Error from './error.js'

// ---------------
// -- Inner Env
// ---------------

export type MakeVar<N, V> = { name: N; value: V };

export type GetVar<
  T
, E> =
  E extends Env
    ? E extends [infer U, ...infer R]
      ? U extends Var
        ? U["name"] extends T
          ? U["value"]
        : GetVar<T, R>
      : TNotMatch
    : TNotMatch
  : TNotMatch

type DelVar<Name extends string, EnvLifo extends Env> = _DelVar<Name, EnvLifo>
type _DelVar<
  Name extends string
, DelEnv extends Env
, R extends Env = []> =
  DelEnv extends []
    ? R
  : DelEnv extends [infer Fst extends Var, ...infer Rest extends Env]
    ? Fst['name'] & Name extends never
      ? _DelVar<Name, Rest, [...R, Fst]>
    : _DelVar<Name, Rest, R>
  : never

// [note]
//   used in `Reading`.
//   If it hits a fn pattern,
//     it deletes the syms in env
//     which are inherited of the previous let-forms.
export type DelEnv<Name extends string, DelEnvLifo extends Env[]> = _DelEnv<Name, DelEnvLifo>
type _DelEnv<
  Name extends string
, DelEnvLifo extends Env[]
, R extends Env[] = []> =
  DelEnvLifo extends []
    ? R
  : DelEnvLifo extends [infer Fst extends Env, ...infer Rest extends Env[]]
    ? _DelEnv<Name, Rest, [...R, DelVar<Name, Fst>]>
  : never

export type ReduceDelEnv<
  Syms extends Sym[]
, ReduceDelEnvLifo extends Env[]> =
  0 extends ReduceDelEnvLifo['length'] | Syms['length']
    ? ReduceDelEnvLifo
  : Syms extends [['sym', infer Fst extends string], ...infer Rest extends Sym[]]
    ? ReduceDelEnv<Rest, DelEnv<Fst, ReduceDelEnvLifo>>
  : never

type EnvLifo = Env[];

export type Let<
  N
, V
, EnvLifo = Env[]> =
  EnvLifo extends Env[]
    ? [...EnvLifo, [MakeVar<N, V>]]
  : Error.LetError0

export type ReadLet<
  N
, EnvLifo = [[]]> =
  EnvLifo extends [...infer HS, infer L]
    ? L extends Env
      ? GetVar<N, L> extends TNotMatch
        ? ReadLet<N, HS>
      : GetVar<N, L>
    : TNotMatch
  : TNotMatch

export type ReadLetRecur<
  Sexpr
, env
, R extends unknown[] = []> =
  Sexpr extends [infer F, ...infer rest]
    ? F extends Sym
      ? ReadAtom<F, env> extends infer P
        ? ReadLetRecur<rest, env, [...R, P extends TNotMatch ? F : P]>
      : never
    : ReadLetRecur<rest, env, [...R, F extends unknown[] ? ReadLetRecur<F, env, []> : F]>
  : R

export type ReadAtom<
  A
, EnvLifo = [[]]
, prev = 0> =
  A extends [`sym`, infer S]
    ? ReadLet<S, EnvLifo>
  : Eval<A, EnvLifo, [prev]>

export type Reading<
  AS
, EnvList = [[]]
, prev = 0
, R = []> =
  R extends Atom[]
    ? AS extends [infer H, ...infer T]
      ? H extends Sym & ['sym', infer _ extends BuiltinsUnion]
        ? Reading<T, EnvList, prev, [...R, H]>
      : H extends Fn & ['fn', infer Syms extends Sym[], infer Body]
        ? Reading<T, EnvList, prev, [...R, ['fn', Syms, ReadLetRecur<Body, ReduceDelEnv<Syms, EnvList extends EnvLifo ? EnvList : never>>]]>
      : H extends Atom
        ? Reading<T, EnvList, prev, [...R, ReadAtom<H, EnvList, prev>]>
      : H extends Sexpr | LetForm | IfForm
        ? Reading<T, EnvList, prev, [...R, Eval<H, EnvList, prev>]>
      : Error.ErrorCase<Error.ReadingError1, "Atom but not able to read.", [EnvList, H]>
    : R
  : Error.ErrorCase<Error.ReadingError0, 'sexpr is not atom list.', R>

// -----------------
// -- String Fn
// -----------------

export type Str<
  S
, R extends string = ""> =
  S extends [infer HS, ...infer T]
    ? Compiler.Unparse<HS> extends infer s extends string
      ? s extends `'${infer inner}'`
        ? Str<T, `${R}${inner}`>
      : Str<T, `${R}${s}`>
    : never
  : [`prim`, `'${R}'`]

export type LispRefind<
  S> =
  S extends [[`prim`, `'${infer regex}'`], [`prim`, `'${infer searched}'`]]
    ? regex.RegexFind<searched, regex> extends [infer _, infer match extends string, infer _]
      ? ['prim', `'${match}'`]
    : ['prim', `''`]
  : S

export type Split<
  Regex extends string
, String extends string> =
  regex.RegexFind<String, Regex> extends [infer prev extends string, infer _match, infer next extends string]
    ? prev extends ''
      ? [...Split<Regex, next>]
    : [['prim', `'${prev}'`], ...Split<Regex, next>]
  : String extends ''
    ? []
  : [['prim', `'${String}'`]]

/**

*/   
export type LispSplit<
  S> =
  S extends [[`prim`, `'${infer searched}'`], [`prim`, `'${infer regex}'`]]
    ? ['vec', ...Split<regex, searched>]
  : Error.LispSplitError0 // [todo]

// [todo] refactoring
type PrimStrWrap<S extends string> = ['prim', `'${S}'`]

// [todo] refactoring
type PrimStrUnwrap<S extends ['prim', string]> = S extends ['prim', `'${infer s}'`] ? s : never

// [todo] refactoring
type StrUnwrap<S extends string> = S extends `'${infer s}'` ? s : S

// [note] this is not in accordance with the spec of Clojure.
export type LiteralReplace<
  String extends string
, Regex extends string
, Replace extends Fn | BuiltinsUnion | ['prim', string]> =
  regex.RegexFind<String, Regex> extends [infer prev extends string, infer match extends string, infer next extends string]
    ? Replace extends (Fn | BuiltinsUnion)
      ? Eval<[Replace, PrimStrWrap<prev>, PrimStrWrap<match>, PrimStrWrap<next>]> extends ['prim', `'${infer EvalR}'`]
        ? LiteralReplace<next, Regex, Replace> extends infer rpl extends string
          ? `${prev}${EvalR}${rpl}`
        : Error.ErrorCase<Error.LiteralReplaceError2, '', [String, Regex, Replace]>
      : [Replace, PrimStrWrap<prev>, PrimStrWrap<match>, PrimStrWrap<next>]
    : Replace extends ['prim', string]
      ? LiteralReplace<next, Regex, Replace> extends infer rpl extends string
        ? `${prev}${PrimStrUnwrap<Replace>}${rpl}`
      : Error.ErrorCase<Error.LiteralReplaceError3, '', [String, Regex, Replace]>
    : Error.ErrorCase<Error.LiteralReplaceError0, '', [String, Regex, Replace]>
  : String

export type LispReplace<
  S> =
  S extends [infer S extends ['prim', string], infer R extends ['prim', string], infer ForS extends (Fn | BuiltinsUnion | ['prim', string])]
    ? LiteralReplace<PrimStrUnwrap<S>, PrimStrUnwrap<R>, ForS> extends infer R
      ? R extends {error: unknown}
        ? R
      : R extends string
        ? ['prim', `'${R}'`]
      : never
    : never
  : Error.ErrorCase<Error.LispReplaceError0, '', S>

export type _StrSubs<
  S extends string
, BitNum extends string
, C extends string = ''> =
  [true & Bit.BitLTE<BitNum, Bit.BitZero>, S & ''] extends [never, never]
    ? S extends `${infer First}${infer Rest}`
      ? _StrSubs<Rest, Bit.BitDec<BitNum>, `${C}${First}`>
    : never
  : [C, S]

export type StrSubsAll<
  S extends string
, N extends string | ratio.Ratio
, M extends string | ratio.Ratio = str.StrLen<S>> =
  [ratio.ForceNat<N>, ratio.ForceNat<M>] extends [infer NatN extends string, infer NatM extends string]
    ? _StrSubs<StrUnwrap<S>,NatN> extends [infer prev extends string, infer tmpMid extends string]
      ? _StrSubs<StrUnwrap<tmpMid>, Bit.BitSub<NatM, NatN>> extends [infer mid extends string, infer post extends string]
        ? [prev, mid, post]
      : never
    : never
  : never

export type _LispStrSubsAll<
  S> =
  S extends [['prim', infer s extends string], ['prim', infer n extends string | ratio.Ratio], ['prim', infer m extends string | ratio.Ratio]]
    ? StrSubsAll<s,n,m> extends [infer prev extends string, infer mid extends string, infer post extends string]
      ? [PrimStrWrap<prev>, PrimStrWrap<mid>, PrimStrWrap<post>]
    : Error.ErrorCase<Error.LispStrSubsAll1, '', S>
  : S extends [['prim', infer s extends string], ['prim', infer n extends string | ratio.Ratio]]
    ? StrSubsAll<s,n> extends [infer prev extends string, infer mid extends string, infer post extends string]
      ? [PrimStrWrap<prev>, PrimStrWrap<mid>, PrimStrWrap<post>]
    : Error.ErrorCase<Error.LispStrSubsAll3, '', S>
  : Error.ErrorCase<Error.LispStrSubsAll0, '', S>

export type LispStrSubsAll<
  S
, idx extends number = -1> =
  _LispStrSubsAll<S> extends infer R
    ? R extends [infer prev extends PrimString, infer mid extends PrimString, infer post extends PrimString]
      ? idx extends 0
        ? prev
      : idx extends 1
        ? mid
      : idx extends 2
        ? post
      : ['vec', prev, mid, post]
    : Error.ErrorCase<Error.LispStrSubsAll2, '', R>
  : never

export type LispCljSubs<S> = LispStrSubsAll<S, 1>

// [todo] move to somewhere
type TrimQuote<S extends string> = S extends `'${infer innerS}'` ? innerS : S extends `"${infer innerS}"` ? innerS : S

export type GetStr<S> = S extends ['prim', infer s extends string] ? TrimQuote<s> : S extends string ? TrimQuote<S> : never

export type Join<
  S extends string
, V extends unknown[]
, Ret extends string = ''> =
  Util.Equal<V['length'], 1> extends true
    ? `${Ret}${GetStr<V[0]>}`
  : V extends [infer H extends unknown, ...infer Rest extends unknown[]]
    ? Join<S, Rest, `${Ret}${GetStr<H>}${GetStr<S>}`>
  : V

// join
export type LispJoin<
  S> =
  S extends [['prim', infer Sep extends string], ['vec', ...infer Rest]]
    ? Util.Equal<Rest, []> extends true
      ? ['prim', `''`]
    : Eval<[['sym', 'map'], ['sym', 'str'], ['vec', ...Rest]]> extends ['vec', ...infer MRest extends unknown[]]
      ? Join<Sep, MRest> extends infer RS extends string
        ? ['prim', `'${RS}'`]
      : 'nn'
    : Error.ErrorCase<Error.LispJoinError3, 'Types of args may be ok but join is failed.', S>
  : S extends unknown[]
    ? S[0] extends ['prim', string]
      ? Error.ErrorCase<Error.LispJoinError0, '2nd should be vector.', S>
    : S[1] extends ['vec', ...infer _Rest extends ['prim', string][]]
      ? Error.ErrorCase<Error.LispJoinError1, '1st should be string.', S>
    : Error.ErrorCase<Error.LispJoinError2, '1st is str & 2nd is vec, but an error occurs.', S>
  : Error.ErrorCase<Error.LispJoinError4, 'Malform Sexpr.', S>

// --------------------------------------------
// -- Logical Operators
// --------------------------------------------

// [todo]
type _And<Fst, Snd> = Fst extends false ? false : Snd extends false ? false : true

type _LispAnd<
  S> =
  S extends [infer Fst, ...infer Rest]
    ? Fst extends Falsy
      ? false
    : Fst extends [`prim`, infer Boolean]
      ? Rest extends []
        ? true
      : _And<Boolean, _LispAnd<Rest>>
    : Error.ErrorCase<Error.LispAndError0, 'Is not prim', S>
  : never

// and
export type LispAnd<
  S> =
  S extends [infer _, ...infer __]
    ? [`prim`, _LispAnd<S>]
  : [`prim`, false]

type _LispOr<
  S> =
  S extends []
    ? false
  : S extends [infer Fst, ...infer Rest]
    ? Fst extends [`prim`, false] | TNil
      ? _LispOr<Rest>
    : true
  : never

// -- or
export type LispOr<
  S> =
  S extends [infer _, ...infer __]
    ? [`prim`, _LispOr<S>]
  : [`prim`, false]

// coll?
type LispIsColl<
  S> =
  S extends infer A extends (Sexpr | Atom)
    ? Util.Equal<LispIsMap<A> & LispIsVector<A>, never> extends true
      ? ['prim', true]
    : ['prim', false]
  : Error.ErrorCase<Error.LispIsCollError0, 'S is not sexpr.', S>

// Define about lisp map equiality.
//
// 1. ConstMapFst and -Snd should keep those values in any recursive layer.
// 2. Compare each elements picked by Get, internal function.
//    If both are TMap, recursively & deeply call this with the two.
// 3. If all branch are equal, return R should be true.
//    If not, return R should be false.
export type _LispCollEq<
  ConstMapFst extends TMap
, ConstMapSnd extends TMap
, Keys extends Keyword[]
, R = false> =
  Keys extends []
    ? { r: R }
  : Keys extends [infer KeyHead extends Keyword, ...infer KeyRest extends Keyword[]]
    ? [Get<KeyHead, ConstMapFst>] extends [infer MapElemFst extends Atom]
      ? [Get<KeyHead, ConstMapSnd>] extends [infer MapElemSnd extends Atom]
        ? [MapElemFst] extends [TMap]
          ? [MapElemSnd] extends [TMap]
            ? LispKeys<[MapElemFst]> extends ['vec', ...infer NextKs extends Keyword[]]
              ? Util.Rec<_LispCollEq<MapElemFst, MapElemSnd, NextKs, false>> extends true
                ? { r: _LispCollEq<ConstMapFst, ConstMapSnd, KeyRest, true> }
              : { r: false }
            : { r: false }
          : { r: false }
        : _LispEq<MapElemFst, MapElemSnd> extends ['prim', true]
          ? { r: _LispCollEq<ConstMapFst, ConstMapSnd, KeyRest, true> }
        : { r: false }
      : { r: false }
    : { r: false }
  : { r: false }

type _RecurLispEq<
  V extends readonly unknown[]
, U extends readonly unknown[]> =
  [V, U] extends [[infer A, ...infer B], [infer C, ...infer D]]
    ? [A, C] extends [['prim', infer ASnd extends ratio.RatioNumber], ['prim', infer BSnd extends ratio.RatioNumber]]
      ? Util.Equal<true, _PrimSndEq<ASnd, BSnd>> extends true
        ? _RecurLispEq<B, D>
      : false
    : Util.Equal<A, C> extends true
      ? Util.Equal<LispIsColl<A>, ['prim', true]> extends true
        ? Util.Equal<_LispEq<A, C>, ['prim', true]> extends true
          ? _RecurLispEq<B, D>
        : false
      : _RecurLispEq<B, D>
    : false
  : V['length'] & U['length'] extends never
    ? false
  : true

type _PrimSndEq<
  X extends string | boolean | ratio.RatioNumber
, Y extends string | boolean | ratio.RatioNumber> =
  [LispIsNumber<[['prim', X]]>, LispIsNumber<[['prim', Y]]>] extends [['prim', true], ['prim', true]]
    ? [X, Y] extends [infer NX extends ratio.RatioNumber, infer NY extends ratio.RatioNumber]
      ? Util.Equal<ratio.SimplifyStr<NX>, ratio.SimplifyStr<NY>>
    : Util.Equal<X, Y>
  : Util.Equal<X, Y>

export type _LispEq<
  Fst
, Snd> =
  [LispIsColl<[Fst]>, LispIsColl<[Snd]>] extends [['prim', true], ['prim', true]]
    ? [Fst, Snd] extends infer V extends [Vector, Vector]
      ? ['prim', _RecurLispEq<V[0], V[1]>]
    : [Fst, Snd] extends [TMap, TMap]
      ? LispKeys<[Fst]> extends ['vec', ...infer RestPickedKeys extends Keyword[]]
        ? ['prim', Util.Rec<_LispCollEq<Extract<Fst, TMap>, Extract<Snd, TMap>, RestPickedKeys>>]
      : never
    : never
  : [Fst, Snd] extends [['prim', infer V extends ratio.RatioNumber], ['prim', infer U extends ratio.RatioNumber]]
    ? ['prim', _PrimSndEq<V, U>]
  : ['prim', Util.Equal<Fst, Snd>]

// builtin: =
export type LispEq<
  S> =
  S extends [ infer Fst extends (Sexpr | Atom)
	    , infer Snd extends (Sexpr | Atom)
	    , ...infer _]
    ? _LispEq<Fst, Snd>
  : Error.ErrorCase<Error.LispEqError0, "", S>

type _Not<B> = B extends false ? true : false

// builtin: not
export type LispNot<S> = S extends [['prim', infer U]] ? ['prim', _Not<U>] : never

// -------------------------------
// -- Bit Operators
// -------------------------------

// builtin: +
export type LispAdd<S> = _LispAdd<S>
export type _LispAdd<
  S
, R extends NumString = Bit.BitZero> =
  S extends []
    ? [`prim`, ratio.ForceRatio<R>]
  : S extends [infer Fst, ...infer Rest]
    ? LispIsNumber<[Fst]> extends ['prim', false]
      ? Error.ErrorCase<Error.LispAddError2, 'Args should be primitive numbers.', [Fst]>
    : Fst extends [`prim`, infer FstP extends NumString]
      ? _LispAdd<Rest, ratio.RatioAdd<ratio.ForceRatio<R>, ratio.ForceRatio<FstP>>>
    : Error.ErrorCase<Error.LispAddError0, "Args should be int or rational numbers.", S>
  : Error.ErrorCase<Error.LispAddError1, "Args should be sexpr.", S>

// builtin: -
export type LispSub<S> = _LispSub<S>
export type _LispSub<
  S
, R extends NumString = Bit.BitZero
, Init extends boolean = true> =
  S extends []
    ? [`prim`, ratio.ForceRatio<R>]
  : LispIsNumber<[S extends Sexpr ? S[0] : never]> extends ['prim', false]
    ? Error.ErrorCase<Error.LispSubError1, 'Args should be primitive numbers.', S>
  : [S, Init] extends [[[`prim`, infer Fst extends NumString]], true]
    ? [`prim`, ratio.RatioNot<Fst>]
  : S extends [[`prim`, infer Fst extends NumString], ...infer Rest extends ['prim', NumString][]]
    ? Init extends true
      ? _LispSub<Rest, Fst, false>
    : ratio.RatioSub<ratio.ForceRatio<R>, ratio.ForceRatio<Fst>> extends infer RatioSub extends RatioString
      ? _LispSub<Rest, RatioSub, false>
    : never
  : Error.ErrorCase<Error.LispSubError0, "Args should be int or rational numbers.", S>

// builtin: inc
export type LispInc<
  S> =
  LispIsNumber<S> extends ['prim', true]
    ? LispAdd<[...S extends [PrimNumber] ? S : never, ['prim', '0000000000000001']]>
  : Error.ErrorCase<Error.LispIncError0, "Args should be primitive numbers.", S>

// builtin: dec
export type LispDec<
  S> =
  LispIsNumber<S> extends ['prim', true]
    ? LispSub<[...S extends [PrimNumber] ? S : never, ['prim', '0000000000000001']]>
  : Error.ErrorCase<Error.LispDecError0, "Args should be primitive numbers.", S>

// builtin: *
export type LispMul<
  S
, R extends NumString = Bit.BitZero
, Init extends boolean = true> =
  S extends []
    ? [`prim`, ratio.ForceRatio<R>]
  : LispIsNumber<[S extends Sexpr ? S[0] : never]> extends ['prim', false]
    ? Error.ErrorCase<Error.LispMulError1, 'Args should be primitive numbers.', S>
  : S extends [[`prim`, infer Fst extends NumString], ...infer Rest extends Atom[]]
    ? Init extends true
      ? LispMul<Rest, Fst, false>
    : LispMul<Rest, ratio.RatioMul<ratio.ForceRatio<R>, ratio.ForceRatio<Fst>>, false>
  : Error.ErrorCase<Error.LispMulError0, "Args should be int or rational numbers.", S>

// builtin: /
export type LispDiv<
  S
, R extends NumString = Bit.BitOne
, Init extends boolean = true> =
  S extends []
    ? [`prim`, ratio.ForceRatio<R>]
  : LispIsNumber<[S extends Sexpr ? S[0] : never]> extends ['prim', false]
    ? Error.ErrorCase<Error.LispDivError2, 'Args should be primitive numbers.', S>
  : S extends [[`prim`, infer Fst extends NumString]
              , ...infer Rest extends Atom[]]
    ? Init extends true
      ? LispDiv<Rest, Fst, false>
    : ratio.RatioDiv<ratio.ForceRatio<R>, ratio.ForceRatio<Fst>> extends infer Div
      ? Div extends 'nil'
        ? TNil
      : Div extends NumString
        ? LispDiv<Rest, Div, false>
      : never
    : Error.ErrorCase<Error.LispDivError0, "Args should be able to be converted into rational numbers.", S>
  : Error.ErrorCase<Error.LispDivError1, "Args should be int or rational numbers.", S>

export type LispTruncOrFloor<
  S
, Mode extends 'trunc' | 'floor'> =
  LispIsNumber<S> extends ['prim', true]
    ? S extends [['prim', infer R extends ratio.Ratio]]
      ? ['prim', Mode extends 'trunc' ? ratio.Trunc<R> : ratio.Floor<R>]
    : S extends [infer ss]
      ? ss
    : never
  : TNil
// builtin: trunc
export type LispTrunc<S> = LispTruncOrFloor<S, 'trunc'>
// builtin: floor
export type LispFloor<S> = LispTruncOrFloor<S, 'floor'>

export type LispRemOrMod<
  S
, Mode extends 'rem' | 'mod'> =
  S extends [infer Fst extends Atom, infer Snd extends Atom]
    ? [['prim', true], ['prim', true]] extends [LispIsNumber<[Fst]>, LispIsNumber<[Snd]>]
      ? ['prim', false] extends LispIsZero<[Snd]>
        ? Eval<[['sym', '/'], Fst, Snd]> extends infer Q extends PrimNumber
          ? Eval<[['sym', '-'], Fst, [['sym', '*'], Eval<[['sym', Mode extends 'rem' ? 'trunc' : 'floor'], Q]>, Snd]]>
        : never
      : TNil
    : Error.ErrorCase<Error.LispRemOrModError2, '', S>
  : Error.ErrorCase<Error.LispRemOrModError1, '', S>
// builtin: rem
export type LispRem<S> = LispRemOrMod<S,'rem'>
// builtin: mod
export type LispMod<S> = LispRemOrMod<S,'mod'>

// builtin: >, <, >=, <=, =
export type LispRelation<
  Name extends '>' | '<' | '>=' | '<=' | '='
, S
, R extends NumString = "00000000"
, Init extends boolean = true
, Next extends boolean = true> =
  S extends []
    ? [`prim`, Next]
  : S extends [[`prim`, infer Fst extends NumString], ...infer Rest extends ['prim', NumString][]]
    ? Init extends true
      ? LispRelation<Name, Rest, Fst, false, Next>
    : Next extends true
      ? ratio.Relation<ratio.ForceRatio<R>, ratio.ForceRatio<Fst>, Name> extends infer Relation extends boolean
        ? LispRelation<Name, Rest, Fst, false, Relation>
      : never
    : [`prim`, false]
  : Error.ErrorCase<Error.LispRelationError1, "", S>

// builtin: if
type If<A, B, C> = A extends [`prim`, false] | TNil ? C : B;

// predicate
// - number?, string?, vector?, map?, fn?, ifn?, pos-int?, neg-int?, odd?, even?, zero?, symbol?, keyword?,  empty? 
type NatNumber = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' // [todo]
type OddNumber = '1'|'3'|'5'|'7'|'9' // [todo]
type IsNumber<
  S extends string> =
  S extends `${infer F}${infer R}`
    ? F extends NatNumber
      ? R extends ''
        ? true
      : IsNumber<R>
    : false
  : false

type _IsOdd<
  S extends string> =
  S extends `${infer F}${infer R}`
    ? R extends ''
      ? F extends OddNumber
        ? true
      : false
    : _IsOdd<R>
  : false

// builtin: odd?
type IsOdd<
  S extends string> =
  true extends IsNumber<S>
    ? _IsOdd<S>
  : false

// builtin: even?
type IsEven<
  S extends string> =
  Util.Equal<IsNumber<S>,true> extends true
    ? Util.Equal<_IsOdd<S>, true> extends true
      ? false
    : true
  : false

// number?
export type LispIsNumber<
  S> =
  S extends [['prim', infer N extends BitString]]
    ? N extends `1` | `0`
      ? ['prim', true]
    : N extends `1${infer rN}`
      ? ['prim', IsNumber<rN>]
    : ['prim', IsNumber<N>]
  : S extends [['prim', infer N extends RatioString]]
    ? ratio.ForceNat<N> extends infer D
      ? D extends 'nil'
        ? ['prim', false]
      : ['prim', true]
    : never
  : ['prim', false]

// string?
export type LispIsString<
  S> =
  S extends [['prim', infer N extends string]]
    ? N extends `'${infer _}'`
      ? ['prim', true]
    : ['prim', false]
  : ['prim', false]

// vector?
export type LispIsVector<
  S> =
  S extends [['vec', ...infer _]]
    ? ['prim', true]
  : ['prim', false]

// map?
export type LispIsMap<
  S> =
  S extends [['map', ...infer _]]
    ? ['prim', true]
  : ['prim', false]

// fn?
export type LispIsFn<
  S> =
  S extends [infer A]
    ? A extends ['fn', ...infer _] | ['sym', BuiltinsFn]
      ? ['prim', true]
    : ['prim', false]
  : ['prim', false]

// keyword?
export type LispIsKeyword<
  S> =
  S extends [['key', infer _]]
    ? ['prim', true]
  : ['prim', false]

// builtin: ifn?
export type LispIsIfn<
  S> =
  Util.Equal<LispIsKeyword<S>, ['prim', true]> extends true
    ? ['prim', true]
  : Util.Equal<LispIsMap<S>, ['prim', true]> extends true
    ? ['prim', true]
  : Util.Equal<LispIsFn<S>, ['prim', true]> extends true
    ? ['prim', true]
  : ['prim', false]

// neg?
export type LispIsNeg<
  S> =
  S extends [['prim', infer N extends BitString]]
    ? N extends `1${infer _}`
      ? LispIsNumber<S>
    : ['prim', false]
  : S extends [['prim', infer N extends RatioString]]
    ? ratio.Commonize<N> extends [infer m extends BitString, infer _n]
      ? m extends `1${infer _}`
        ? LispIsNumber<S>
      : ['prim', false]
    : never
  : ['prim', false]

// pos?
export type LispIsPos<
  S> =
  S extends [['prim', infer N extends BitString]]
    ? N extends `1${infer _}`
      ? ['prim', false]
    : LispIsNumber<S>
  : S extends [['prim', infer N extends RatioString]]
    ? ratio.Commonize<N> extends [infer m extends BitString, infer _n]
      ? m extends `1${infer _}`
        ? ['prim', false]
      : LispIsNumber<S>
    : never
  : ['prim', false]

// int?
export type LispIsInt<
  S> =
  S extends [['prim', infer _N extends BitString]]
    ? LispIsNumber<S>
  : S extends [['prim', infer N extends RatioString]]
    ? Util.Equal<ratio.IsInt<N>, true> extends true
      ? ['prim', true]
    : ['prim', false]
  : ['prim', false]

// nat?
export type LispIsNat<
  S> =
  [Util.Equal<LispIsInt<S> extends infer a?a:never, ['prim', true]>, Util.Equal<LispIsNeg<S> extends infer a?a:never, ['prim', false]>] extends [true, true]
    ? ['prim', true]
  : ['prim', false]

// pos-int?
export type LispIsPosInt<
  S> =
  [Util.Equal<LispIsInt<S> extends infer a?a:never, ['prim', true]>, Util.Equal<LispIsPos<S> extends infer a?a:never, ['prim', true]>] extends [true, true]
    ? ['prim', true]
  : ['prim', false]

// neg-int?
export type LispIsNegInt<
  S> =
  [Util.Equal<LispIsInt<S> extends infer a?a:never, ['prim', true]>, Util.Equal<LispIsNeg<S> extends infer a?a:never, ['prim', true]>] extends [true, true]
    ? ['prim', true]
  : ['prim', false]

// odd?
export type LispIsOdd<
  S> =
  S extends [['prim', infer N extends BitString]]
    ? ['prim', IsOdd<N>]
  : S extends [['prim', infer N extends RatioString]]
    ? ratio.ForceNat<N> extends infer D extends string
      ? D extends 'nil'
        ? ['prim', false]
      : ['prim', IsOdd<D>]
    : never
  : ['prim', false]

// even?
export type LispIsEven<
  S> =
  S extends [['prim', infer N extends string]]
    ? ['prim', IsEven<N>]
  : S extends [['prim', infer N extends RatioString]]
    ? Util.Equal<ratio.IsInt<N>,false> extends true
      ? ['prim', false]
    : ratio.ForceNat<N> extends infer D extends string
      ? D extends 'nil'
        ? ['prim', false]
      : ['prim', IsEven<D>]
    : never
  : ['prim', false]

// zero?
export type LispIsZero<
  S> =
  S extends [['prim', infer N extends string]]
    ? Util.Equal<Bit.BitIsZero<N>, true> extends true
      ? N extends 'nil'
        ? ['prim', false]
      : ['prim', true]
    : ['prim', false]
  : S extends [['prim', infer N extends RatioString]]
    ? ['prim', ratio.RatioIsZero<N>]
  : ['prim', false]

// ratio?
export type LispIsRatio<
  S> =
  S extends [['prim', infer _N extends RatioString]]
    ? Util.Equal<LispIsInt<S> extends infer a ? a : never, ['prim', false]> extends true
      ? ['prim', true]
    : ['prim', false]
  : ['prim', false]

// symbol?
export type LispIsSymbol<
  S> =
  S extends [['sym', infer _]]
    ? ['prim', true]
  : ['prim', false]

// empty?
export type LispIsEmpty<
  S> =
  S extends [['vec', ...infer V]]
    ? V extends []
      ? ['prim', true]
    : ['prim', false]
  : S extends [['map', []]]
    ? ['prim', true]
  : ['prim', false]

// boolean?
export type LispIsBoolean<
  S> =
  S extends [['prim', true]] | [['prim', false]]
    ? ['prim', true]
  : ['prim', false]

// any?
export type LispIsAny<
  S> =
  S extends Sexpr
    ? ['prim', true]
  : Error.ErrorCase<Error.LispIsAnyError0, "any? always return `true` but this sexpr has maybe error objects.", S>

// builtin: prim?
export type LispIsPrim<
  S> =
  S extends [['prim', infer prim]]
    ? prim extends NilLiteral
      ? ['prim', false]
    : ['prim', true]
  : ['prim', false]

// builtin: type
export type LispType<
  S> =
  LispIsNumber<S> extends ['prim', true]
    ? ['prim', `'number'`]
  : LispIsString<S> extends ['prim', true]
    ? ['prim', `'string'`]
  : LispIsMap<S> extends ['prim', true]
    ? ['prim', `'map'`]
  : LispIsVector<S> extends ['prim', true]
    ? ['prim', `'vector'`]
  : LispIsFn<S> extends ['prim', true]
    ? ['prim', `'fn'`]
  : LispIsBoolean<S> extends ['prim', true]
    ? ['prim', `'boolean'`]
  : LispIsKeyword<S> extends ['prim', true]
    ? ['prim', `'key'`]
  : LispIsSymbol<S> extends ['prim', true]
    ? S extends [['sym', BuiltinsFn]]
      ? ['prim', `'fn'`]
    : ['prim', `'symbol'`]
  : ['prim', `'nil'`]

// ----------------------
// -- collection mod
// ----------------------

export type TConcat<
  V extends Array<Array<unknown>>
, Stack extends Array<unknown> = []> =
  V['length'] extends 0
    ? Stack
  : V extends [infer Head extends Array<unknown>, ...infer Rest extends Array<Array<unknown>>]
    ? TConcat<Rest, [...Stack, ...Head]>
  : never

export type VCons<
  V
, R extends unknown[] = []> =
  V extends ['vec', infer v, infer vv]
    ? VCons<vv, [...R, v]>
  : V extends ['vec', infer v]
    ? VCons<VecEmpty, [...R, v]>
  : V extends VecEmpty
    ? ['vec', ...R]
  : ['vec', ...R, V]

// bulitin: concat
export type LispConcat<
  S
, R extends unknown[][] = []> =
  S extends Vector[] & [['vec', ...infer H], ...infer T]
    ? T extends []
      ? ['vec', ...TConcat<[...R, H]>]
    : LispConcat<T, [...R, H]>
  : Error.ErrorCase<Error.ConcatError0, '', S>

// builtin: keyword?
type IsKeyword<
  T> =
  T extends ['key', `:${infer S}`]
    ? true
  : false

type IsMap<T> = T extends TMap ? true : false

export type IsKeyMapSexpr<
  S
, env = [[]]> =
  S extends [infer Fst, infer Snd]
    ? IsKeyword<Eval<Fst, env>> extends true
      ? IsMap<Eval<Snd, env>> extends true
        ? true
      : false
    : IsKeyword<Eval<Snd, env>> extends true
      ? IsMap<Eval<Fst,env>> extends true
        ? true
      : false
    : false
  : false

export type GetMap<
  K
, V
, sV = V extends [infer _, infer i] ? i : never> =
  sV extends [infer k, infer v, ... infer _]
    ? k extends K
      ? v
    : GetMap<K,V,sV extends [infer _, infer __, ...infer i] ? i : never>
  : TNil

type GetVec<
  Idx extends PrimNumber
, Vec extends Vector> =
  Vec extends ['vec', infer H, ...infer T extends Atom[]]
    ? Idx extends PrimNumber & ['prim', infer idx extends string]
      ? Bit.BitIsZero<idx> extends true
        ? H
      : Bit.BitGT<idx, Bit.BitZero> extends true
        ? T extends []
          ? TNil
        : GetVec<['prim', Bit.BitSub<idx, "1">], ['vec', ...T]>
      : Error.ErrorCase<Error.GetVecError0, '', [Idx, Vec]>
    : Error.ErrorCase<Error.GetVecError1, '', [Idx, Vec]>
  : TNil

export type Get<
  K extends PrimNumber | Keyword
, V extends Vector | TMap> =
  V extends Vector
    ? K extends PrimNumber
      ? GetVec<K, V>
    : Error.ErrorCase<Error.GetError0, '', [K,V]>
  : GetMap<K, V>

export type LispGet<
  S> =
  S extends [infer Map extends TMap, infer Key extends Keyword]
    ? Get<Key, Map>
  : S extends [infer Vec extends Vector, infer Idx extends ['prim', BitString]]
    ? Get<Idx, Vec>
  : S extends [infer Vec extends Vector, infer Idx extends ['prim', NumString]]
    ? ratio.ForceNat<Idx[1]> extends infer D extends string
      ? D extends 'nil' // [todo]
        ? TNil
      : Get<D extends PrimNumber ? D : never, Vec>
    : never
  : S extends [infer Map extends TMap, infer Idx extends PrimNumber]
    ? TNil
  : S extends [infer MNil extends TNil | VecEmpty | ['map'], infer _]
    ? TNil
  : Error.ErrorCase<Error.LispGetError0, "this is not map and key or vector and idx-num.", S>

export type LispGetIn<
  S> =
  S extends [infer f, infer s]
    ? s extends ['vec', infer j, ...infer k]
      ? k extends []
        ? LispGet<[f, j]>
      : LispGetIn<[LispGet<[f, j]>, ['vec', ...k]]>
    : Error.ErrorCase<Error.LispGetInError1, 'the second should be a vector.', S>
  : Error.ErrorCase<Error.LispGetInError0, '', S>

export type LispSecond<
  S> =
  S extends [Vector]
    ? LispGet<[...S, ['prim', Bit.BitOne]]>
  : Error.ErrorCase<Error.LispSecondError0, "arg should be a vector.", S>

export type _Assoc<
  M
, K extends Keyword | PrimNumber
, V extends Atom
, Type extends 'assoc' | 'update' = 'assoc'
, S extends unknown[] = []> =
  M extends Vector & ['vec', infer mV extends Atom, ...infer mR extends Atom[]]
    ? K extends PrimNumber & ['prim', infer kB extends string]
      ? Bit.BitIsZero<kB> extends true
        ? ['vec', ...S, Type extends 'update' ? Eval<[V, mV]> : V, ...mR]
      : Bit.BitGT<kB, Bit.BitZero> extends true
        ? mR extends []
          ? TNil
        : _Assoc<['vec', ...mR], ['prim', Bit.BitSub<kB, Bit.BitOne>], V, Type, [...S, mV]>
      : ['vec', ...S, V, ...mR]
    : Error.ErrorCase<Error.AssocError1, Error.AssocErrorMsg1, M>
  : M extends TMap & ['map', [infer mK extends Keyword, infer mV extends Atom, ...infer mR]]
    ? mK extends K
      ? ['map', [...S, mK, Type extends 'update' ? Eval<[V, mV]> : V, ...mR]]
    : mR extends []
      ? TNil
    : _Assoc<['map', mR], K, V, Type, [...S, mK, mV]>
  : TNil

type _rAssocIn<
  M  extends Vector | TMap
, Kh extends (Keyword | PrimNumber)
, Kt extends (Keyword | PrimNumber)[]
, V  extends Atom
, Type extends 'update' | 'assoc' = 'assoc'> =
  Kt extends []
    ? _Assoc<M, Kh, V, Type>
  : Get<Kh, M> extends infer Next
    ? Next extends Vector | TMap
      ? _AssocIn<Next, ['vec', ...Kt], V, Type> extends infer Recur
        ? Recur extends Error.AccessFailed
          ? Error.AccessFailed
        : Recur extends TNil
          ? Error.AccessFailed
        : Recur extends Atom
          ? _Assoc<M, Kh, Recur>
        : Error.ErrorCase<Error.AssocInError7, `The value of key (${Kt[0][1] extends RatioString ? `${Kt[0][1][0]}/${Kt[0][1][1]}` : Kt[0][1] extends string ? Kt[0][1] : never}) is not vector nor map.`, M>
      : Error.ErrorCase<Error.AssocInError3, '', M> // [todo]
    : Error.AccessFailed
  : Error.ErrorCase<Error.AssocInError4, "", M> // [todo]
 
// [note]
// - This accepts only a keyword if the element is map, Clojure can take it though.
export type _AssocIn<
  M extends Vector | TMap
, Ks extends ['vec', ...(Keyword | PrimNumber | ['prim', RatioString])[]]
, V extends Atom
, Type extends 'update' | 'assoc' = 'assoc'> =
  M extends Vector
    ? M extends VecEmpty
      ? M
    : Ks extends ['vec', infer Kh extends ['prim', RatioString], ...infer Kt extends (Keyword | PrimNumber | ['prim', RatioString])[]]
      ? ratio.ForceNat<Kh[0]> extends infer D extends string
        ? D extends NilLiteral
          ? TNil
        : _rAssocIn<M, ['prim', D], Kt, V, Type>
      : never
    : Ks extends ['vec', infer Kh extends PrimNumber, ...infer Kt extends (Keyword | PrimNumber | ['prim', RatioString])[]]
      ? _rAssocIn<M, Kh, Kt, V, Type>
    : Error.AssocInError0 // [todo]
  : M extends TMap
    ? Ks extends ['vec', infer Kh extends Keyword, ...infer Kt extends (Keyword | PrimNumber | ['prim', RatioString])[]]
      ? _rAssocIn<M, Kh, Kt, V, Type>
    : TNil
  : Error.AssocInError6 // [todo]

export type _Update<
  M
, K extends Keyword | PrimNumber | ['prim', RatioString]
, F extends Fn | ['sym', BuiltinsFn]> =
  K extends ['prim', RatioString]
    ? ratio.ForceNat<K[0]> extends infer D extends string
      ? D extends 'nil'
        ? TNil
      : _Assoc<M, ['prim', D], F, 'update'>
    : never
  : _Assoc<M, K, F, 'update'>

export type _UpdateIn<
  M extends Vector | TMap
, K extends ['vec', ...(Keyword | PrimNumber | ['prim', RatioString])[]]
, F extends Fn | ['sym', BuiltinsFn]> =
_AssocIn<M, K, F, 'update'>

// assoc
export type LispAssoc<
  S> =
  S extends [ infer M extends Vector | TMap
  , infer K
  , infer V extends Atom]
    ? K extends ['prim', RatioString]
      ? ratio.ForceNat<K[0]> extends infer D extends string
        ? D extends NilLiteral
          ? TNil
        : _Assoc<M,['prim', D],V>
      : never
    : K extends Keyword | PrimNumber
      ? _Assoc<M,K,V>
    : never
  : Error.ErrorCase<Error.LispAssocError0, Error.LispAUErrorMsg, S>

// assoc-in
export type LispAssocIn<
  S> =
  S extends [ infer M extends Vector | TMap
  , infer Ks extends ['vec', ...(Keyword | PrimNumber | ['prim', RatioString])[]]
  , infer V extends Atom]
    ? _AssocIn<M,Ks,V> extends infer Return
      ? Util.Equal<Return, Error.AccessFailed> extends true
        ? TNil
      : Return
    : never
  : Error.ErrorCase<Error.LispAssocInError0, Error.LispAUErrorMsg, S>

// update
export type LispUpdate<
  S> =
  S extends [ infer M extends Vector | TMap
  , infer K extends Keyword | PrimNumber | ['prim', RatioString]
  , infer V extends Fn | ['sym', BuiltinsFn]]
    ? _Update<M,K,V>
  : Error.ErrorCase<Error.LispUpdateError0, Error.LispAUErrorMsg, S>

// update-in
export type LispUpdateIn<
  S> =
  S extends [ infer M extends Vector | TMap
  , infer Ks extends ['vec', ...(Keyword | PrimNumber | ['prim', RatioString])[]]
  , infer V extends Fn | ['sym', BuiltinsFn]]
    ? _UpdateIn<M,Ks,V> extends infer Return
      ? Util.Equal<Return, Error.AccessFailed> extends true
        ? TNil
      : Return
    : never
  : S extends [infer M, infer Ks, infer _V]
    ? M extends Vector | TMap
      ? Ks extends ['vec', ...(Keyword | PrimNumber | ['prim', RatioString])[]]
        ? Error.ErrorCase<Error.LispUpdateInError3, "The 3rd must be fn.", S>
      : Error.ErrorCase<Error.LispUpdateInError2, "The 2st must be key vec.", S>
    : Error.ErrorCase<Error.LispUpdateInError1, "The 1st must be vec or map.", S>
  : Error.ErrorCase<Error.LispUpdateInError0, "", S>

export type LispVector<S> =
  S extends unknown[]
    ? ['vec', ...S]
  : Error.ErrorCase<Error.LispVectorError0, `Sexpr's inner expression is not array.`, S>

export type Count<
  S extends unknown[]
, I extends string = Bit.BitZero> =
  S extends [infer _, ...infer R]
    ? R extends []
      ? Bit.BitAdd<I, Bit.BitOne>
    : Count<R, Bit.BitAdd<I, Bit.BitOne>>
  : Error.ErrorCase<Error.CountError0, '1st should be an array as an inner expression.', S>

export type LispCount<
  S> =
  S extends [Vector] & [VecEmpty]
    ? ['prim', Bit.BitZero]
  : S extends [Vector] & [['vec', ...infer V]]
    ? ['prim', Count<V>]
  : Error.ErrorCase<Error.CountError1, 'Arg of count should be vector.', S>

type Zipmap<
  KS
, VS> =
  [KS, VS] extends [[infer fstK, ...infer restK], [infer fstV, ...infer restV]]
    ? [fstK, fstV, ...Zipmap<restK, restV>]
  : []

type LispZipmap<
  S> =
  S extends [['vec', ...infer VecKeys], ['vec', ...infer VecValues]]
    ? ['map', Zipmap<VecKeys, VecValues>]
  : Error.ErrorCase<Error.LispZipmapError0, '', S>

type LispApply<
  S> =
  S extends [infer F extends Fn | ['sym', BuiltinsFn], ['vec', ...infer V]]
    ? Eval<[F, ...V]>
  : Error.ErrorCase<Error.LispApplyError0, "", S>

// ------------
// -- getter
// ------------

export type First<
  V> =
  V extends Vector & [`vec`, infer H, ...infer _]
    ? H
  : V extends VecEmpty // [todo]
    ? TNil
  : Error.ErrorCase<Error.FirstError0, Error.CommonArgVecErrMsg, V>
export type LispFirst<S> = S extends [infer V extends Vector] ? First<V> : Error.ErrorCase<Error.FirstError1, Error.CommonArgVecErrMsgFn<'first'>, S>

type Last<
  V> =
  V extends [infer H, ...infer T]
    ? T extends []
      ? H
    : Last<T>
  : Error.ErrorCase<Error.LastError0, Error.CommonArgVecErrMsg, V>
export type LispLast<S> = S extends [VecEmpty] ? TNil : S extends [['vec', ...infer V]] ? Last<V> : Error.ErrorCase<Error.LastError1, Error.CommonArgVecErrMsgFn<'last'>, S>

export type Rest<
  V> =
  V extends Vector & [`vec`, infer _, ...infer T]
    ? T[0] extends Atom
      ? [`vec`, ...T]
    : VecEmpty
  : Error.ErrorCase<Error.RestError0, Error.CommonArgVecErrMsg, V>

/**
Builtin: `rest`.
*/
export type LispRest<
  S> =
  S extends [infer V extends Vector]
    ? Util.Equal<V,VecEmpty> extends true
      ? TNil
    : Rest<V>
  : Error.ErrorCase<Error.RestError1, Error.CommonArgVecErrMsgFn<'rest'>, S>

type _Butlast<
  V
, R extends unknown[] = []> =
  V extends [infer H, ...infer T]
    ? T extends []
      ? R
    : _Butlast<T,[...R,H]>
  : Error.ErrorCase<Error.ButlastError0, Error.CommonArgVecErrMsg, V>

export type Butlast<V> =
  V extends Vector & ['vec', ...infer v]
    ? _Butlast<v> extends Atom[]
      ? ['vec', ..._Butlast<v>]
    : Error.ErrorCase<Error.ButlastError1, Error.CommonArgVecErrMsg, V>
  : Error.ErrorCase<Error.ButlastError3, Error.CommonArgVecErrMsg, V>

// butlast
export type LispButlast<S> =
  S extends [infer V extends Vector]
    ? Util.Equal<V,VecEmpty> extends true
      ? TNil
    : Butlast<V>
  : Error.ButlastError2

// -------------
// -- new seq
// -------------

export type Conj<
  V
, E> =
  E extends Atom
    ? V extends Vector
      ? [...V, E]
    : Error.ErrorCase<Error.ConjError0, '1st should be vector', [V,E]>
  : Error.ErrorCase<Error.ConjError0, '2nd should be Atom', [V,E]>

// builtin: conj
type LispConj<
  S> =
  S extends [infer H extends Vector, ...infer T extends Atom[]]
    ? [...H, ...T]
  : Error.ErrorCase<Error.ConjError2, Error.CommonArgVecErrMsgFn<'conj'>, S>

export type Concat<
  V
, W> =
  V extends Vector
    ? W extends Vector & [`vec`, ...infer WW]
      ? [...V, ...WW]
    : Error.ErrorCase<Error.ConcatError, '1st should be vector', [V,W]>
  : Error.ErrorCase<Error.ConcatError, '2nd should be vector', [V,W]>

export type Take<
  N extends NumString
, V extends unknown[]
, R extends unknown[] = []> =
  N extends string
    ? V extends []
      ? R
    : Bit.BitGTE<Bit.BitZero, N> extends true
      ? R
    : V extends [infer F, ...infer T]
      ? Take<Bit.BitSub<N, Bit.BitOne>, T, [...R, F]>
    : Error.ErrorCase<Error.TakeError0, '2nd should be an array.', [N,V,R]>
  : N extends RatioString
    ? ratio.ForceNat<N> extends infer D extends string
      ? D extends NilLiteral
        ? TNil
      : Take<D, V, R>
    : never
  : never

// builtin: take
type LispTake<
  S> =
  S extends [['prim', infer N extends NumString], ['vec', ...infer V]]
    ? Take<N,V> extends infer RV
      ? RV extends unknown[]
        ? ['vec', ...RV]
      : Error.ErrorCase<Error.TakeError2, 'take spits an inner error', RV>
    : never
  : Error.ErrorCase<Error.TakeError1, '1st and 2nd should be a number and a vector.', S>

/** builtin: drop */
export type Drop<
  N extends NumString
, V extends unknown[]
, R extends unknown[] = []> =
  N extends string
    ? V extends []
      ? R
    : V extends [infer _, ...infer T]
      ? Bit.BitGTE<Bit.BitZero, N> extends true
        ? V
      : Drop<Bit.BitSub<N, Bit.BitOne>, T>
    : Error.ErrorCase<Error.DropError0, '2nd should be vector', [N,V]>
  : N extends RatioString
    ? ratio.ForceNat<N> extends infer D extends string
      ? D extends NilLiteral
        ? TNil
      : Drop<D, V, R>
    : never
  : never

// builtin: drop
type LispDrop<
  S> =
  S extends [['prim', infer N extends NumString], ['vec', ...infer V]]
    ? Drop<N,V> extends infer RV
      ? RV extends unknown[]
        ? ['vec', ...RV]
      : Error.ErrorCase<Error.DropError2, 'drop spits an inner error', RV>
    : never
  : Error.ErrorCase<Error.DropError1, '1st and 2nd should be a number and a vector.', S>

/**
Defines `LispMin` and `LispMax` at once.
*/
type _LispMinMax<
  S
, W
, E extends string> =
  S extends Sexpr
    ? Eval<[['sym', 'reduce'], ['fn', [['sym', 'return'], ['sym', 'i']], ['if', [['sym', W], ['sym', 'return'], ['sym', 'i']], ['sym', 'return'], ['sym', 'i']]], S[0], ['vec', ...S]]>
  : Error.ErrorCase<E, '', S>

// builtin: min
type LispMin<S> = _LispMinMax<S, '<=', Error.LispMinError0>
// builtin: max
type LispMax<S> = _LispMinMax<S, '>=', Error.LispMinError0>


// -------------------------------------
// -- map, filter, remove, every, some
// -------------------------------------

type _FMap<
  F
, V> =
  V extends Vector
    ? V extends [`vec`, infer H, ...infer T]
      ? T[0] extends Atom
        ? [Eval<[F, H]>, ..._FMap<F, [`vec`, ...T]>]
      : [Eval<[F, H]>]
    : []
  : []
type FMap<F, V> = [`vec`, ..._FMap<F, V>];
type LispMap<
  S> =
  S extends [infer f, infer vs]
    ? FMap<f, vs>
  : Error.ErrorCase<Error.FMapError, 'map should have 2 args', S>

type _Filter<
  F
, V> =
  V extends Vector & [`vec`, infer H, ...infer T]
    ? T extends []
      ? Eval<[F, H]> extends [`prim`, true]
        ? [H]
      : []
    : Eval<[F, H]> extends [`prim`, true]
      ? [H, ..._Filter<F, [`vec`, ...T]>]
    : [..._Filter<F, [`vec`, ...T]>]
  : [Error.ErrorCase<Error.FilterError0, "2nd should be vector", [F, V]>]

export type Filter<F, V> = V extends VecEmpty ? VecEmpty : [`vec`, ..._Filter<F, V>];

// builtin: filter
export type LispFilter<
  S> =
  S extends [infer f, infer vs]
    ? Filter<f, vs>
  : Error.ErrorCase<Error.FilterError1, "filter should have 2 args.", S>

// builtin: remove
export type LispRemove<
  S> =
  S extends [infer f, infer vs]
    ? Filter<['fn', [['sym', 'aaa']], [['sym', 'not'], [f, ['sym', 'aaa']]]], vs>
  : Error.ErrorCase<Error.RemoveError0, "remove should have 2 args.", S>

// builtin: every?
export type LispIsEvery<
  S> =
  S extends [infer f, infer vs]
    ? vs extends VecEmpty
      ? ['prim', false]
    : Eval<[['sym', '='], [['sym', 'filter'], f, vs], vs]>
  : Error.ErrorCase<Error.EveryError0, 'every should have 2 args.', S>

// builtin: some
export type LispSome<
  S> =
  S extends [infer f, infer vs]
    ? vs extends VecEmpty
      ? ['prim', false]
    : Eval<[['sym', '->>'], vs, [['sym', 'filter'], f], ['sym', 'count'], ['sym', 'zero?'], ['sym', 'not']]>
  : Error.ErrorCase<Error.EveryError0, 'every should have 2 args.', S>

export type LispIsNil<
  S> =
  S extends [infer A]
    ? A extends TNil
      ? ['prim', true]
    : ['prim', false]
  : Error.ErrorCase<Error.LispIsNilError0, '[compile error] S should be wraped with a taple.', S>

export type LispIsSome<
  S> =
  S extends [infer A]
    ? A extends TNil
      ? ['prim', false]
    : ['prim', true]
  : Error.ErrorCase<Error.LispIsSomeError0, '[compile error] S should be wraped with a taple.', S>

export type Interleave<
  V
, W> =
  V extends [infer HeadV, ...infer TailV]
    ? W extends [infer HeadW, ...infer TailW]
      ? TailW extends never
        ? []
      : TailV extends never
        ? []
      : [HeadV, HeadW, ...Interleave<TailV, TailW>]
    : []
  : []

// builtin: interleave
export type LispInterleave<S> =
  S extends [['vec', ...infer V], ['vec', ...infer W]]
    ? ['vec', ...Interleave<V, W>]
  : Error.ErrorCase<Error.InterleaveError1, 'interleave should have 2 vector.', S> 

type _Nui<
  S extends unknown[]
, V extends unknown
, Where extends 0 | 1
, R extends unknown[] = []> =
  S extends [infer F, ...infer T]
    ? _Nui<T, V, Where, [...R, (Where extends 0 ? [V, F] : [F, V])]>
  : R

export type Nui<S extends unknown[], V extends unknown, Where extends 0|1> = _Nui<S, V, Where>

export type GetKV<
  S extends TMap | Vector> =
  S extends ['map', []] | VecEmpty
    ? []
  : S extends ['map', [infer K extends Keyword, infer V extends Atom, ...infer Rest extends Atom[]]]
    ? GetKV<['map', Rest]> extends infer Result extends (Keyword | PrimNumber)[]
      ? [K, ...Result]
    : [K]
  : S extends ['vec', ...infer Rest extends Atom[]]
    ? [Decimal.DecimalToBit<`${Rest['length']}`>] extends [infer Length extends string]
      ? Range<Bit.BitZero, Length> extends infer Idxes extends string[]
        ? Nui<Idxes, 'prim', 0>
      : never
    : never
  : Error.ErrorCase<Error.LispKeysError0,'',S>

export type LispKeys<
  S> =
  S extends [infer M extends TMap | Vector]
    ? GetKV<M> extends infer KS
      ? KS extends (Keyword | PrimNumber)[]
        ? ['vec', ...KS]
      : KS
    : never
  : S extends {error: unknown}
    ? Error.ErrorCase<Error.LispKeysError1, '', S>
  : TNil

// bulitin: third
export type LispThird<
  S> =
  S extends [['vec', infer _, infer _, infer V, ...infer _R]]
    ? V
  : TNil

// builtin: abs
export type LispAbs<
  S> =
  S extends [TNil]
    ? TNil
  : S extends [['prim', `'${infer _}`]]
    ? TNil
  : S extends [['prim', infer N extends ratio.RatioNumber]]
    ? ['prim', ratio.RatioAbs<N>]
  : Error.ErrorCase<Error.LispAbsError0, 'Not Number.', S>

// builtin: repeat
export type LispRepeat<
  S> =
  S extends [['prim', infer N extends ratio.RatioNumber], infer V extends Each]
    ? ratio.ForceNat<N> extends infer Nat extends ratio.Nat
      ? ['vec', ...Vec.RepeatByBit<Nat, V>]
    : Error.ErrorCase<Error.LispRepeatError1, `Cast failure.`, S>
  : Error.ErrorCase<Error.LispRepeatError0, '', S>

export type _Range<
  N extends ratio.Nat
, M extends ratio.Nat
, R extends ratio.Nat[] = []> =
  Bit.BitGTE<N, M> extends false
    ? _Range<Bit.BitInc<N>, M, [...R, N]>
  : R

export type Range<
  N extends ratio.RatioNumber
, M extends ratio.RatioNumber> =
  [ratio.ForceNat<N>, ratio.ForceNat<M>] extends [infer n extends ratio.Nat, infer m extends ratio.Nat]
    ? _Range<n,m>
  : never

// builtin: range
export type LispRange<
  S> =
  S extends [['prim', infer N extends ratio.RatioNumber], ['prim', infer M extends ratio.RatioNumber]]
    ? Range<N,M> extends infer r extends unknown[]
      ? ['vec', ...Nui<r, 'prim', 0>]
    : Error.ErrorCase<Error.LispRangeError1, 'Range broken.', S>
  : Error.ErrorCase<Error.LispRangeError0, 'Both should be number.', S>

type _Reduce<
  F
, Init
, V> =
  V extends [infer H, ...infer T]
    ? T['length'] extends 0
      ? Eval<[F, Init, H]>
    : _Reduce<F, Eval<[F, Init, H]>, T>
  : Error.ErrorCase<Error.ReduceError0, '', [F,Init,V]>

export type Reduce<
  F
, Init
, V> =
  V extends ['vec', ...infer v]
    ? _Reduce<F,Init,v>
  : Error.ErrorCase<Error.ReduceError1, '', [F,Init,V]>

// builtin: reduce
export type LispReduce<
  S> =
  S extends [infer f, infer init, infer v]
    ? Reduce<f,init,v>
  : Error.ErrorCase<Error.ReduceError2, 'reduce should have 3 args.', S>

export type Reverse<
  V
, R extends Array<unknown> = []> =
  V extends [infer H, ...infer T]
    ? T['length'] extends 0
      ? [H, ...R]
    : Reverse<T, [H, ...R]>
  : V extends []
    ? []
  : { error: [Error.ReverseError0] }

// builtin: reverse
type LispReverse<S> =
  S extends [Vector] & [['vec', ...infer V]]
    ? Reverse<V> extends infer RV
      ? RV extends unknown[]
        ? ['vec', ...RV]
      : Error.ErrorCase<Error.ReverseError1, 'reverse error', RV>
    : never
  : Error.ErrorCase<Error.ReverseError3, 'reverse should have 1 vector.', S>

// note : for threading macros: insertsecond, insertlast, vecwrap
export type InsertSecond<
  V
, E> =
  V extends [infer H, ...infer R]
    ? [H, E, ...R]
  : V extends [...infer R]
    ? [E, ...R]
  : Error.ErrorCase<Error.InsertSecondError0, '', [V,E]>

export type InsertLast<
  V
, E> =
  V extends [...infer R]
    ? [...R, E]
  : Error.ErrorCase<Error.InsertLastError0, '', [V,E]>

// type VecWrapError0 = 'VecWrapError0'
// note : any sexpr and any atom of them should be rendered 
//        such as [['sym', 'inc'], ['prim', '0']] and ['prim', '0'].
type VecWrap<V> = V extends unknown[][] ? V : [V]

export type ThreadFirst<
  Fst
, V extends unknown[]
, R extends unknown[] = []
, Init extends boolean = true> =
  V['length'] extends 0
    ? InsertSecond<VecWrap<Fst>, R>
  : V extends [infer Head, ...infer Tail]
    ? Init extends false
      ? ThreadFirst<Head, Tail, InsertSecond<VecWrap<Fst>, R>, false>
    : Tail extends [infer N, ...infer M]
      ? ThreadFirst<N, M, InsertSecond<VecWrap<Head>, Fst>, false>
    : InsertSecond<VecWrap<Head>, Fst>
  : Error.ErrorCase<Error.ThreadFirstError1, '', [Fst, V, R, Init]>

export type LispThreadFirst<
  S> =
  S extends [infer H, ...infer T]
    ? T['length'] extends 0
      ? S
    : ThreadFirst<H,T>
  : Error.ErrorCase<Error.LispThreadFirstError0, '-> should have 1 elem', S>

export type ThreadLast<
  Fst
, V extends unknown[]
, R extends unknown[] = []
, Init extends boolean = true> =
  V['length'] extends 0
    ? InsertLast<VecWrap<Fst>, R>
  : V extends [infer Head, ...infer Tail]
    ? Init extends false
      ? ThreadLast<Head, Tail, InsertLast<VecWrap<Fst>, R>, false>
    : Tail extends [infer N, ...infer M]
      ? ThreadLast<N, M, InsertLast<VecWrap<Head>, Fst>, false>
    : InsertLast<VecWrap<Head>, Fst>
  : Error.ErrorCase<Error.ThreadLastError1, '', [Fst, V, R]>

export type LispThreadLast<
  S> =
  S extends [infer H, ...infer T]
    ? T['length'] extends 0
      ? S
    : ThreadLast<H,T>
  : Error.ErrorCase<Error.LispThreadLastError0, '->> should have 1 elem', S>

export type RegenFn<
  Body extends (Sexpr|Each)
, InsertP extends 'second' | 'last'> =
(InsertP extends 'second'
     ? ['fn', [['sym', 'x']], ThreadFirst<['sym', 'x'], [Body]>]
   :  ['fn', [['sym', 'x']], ThreadLast<['sym', 'x'], [Body]>])

export type SomeThreadGeneral<
  Fst
, V extends (Sexpr|Each)[]
, InsertP extends 'second' | 'last'> =
  V['length'] extends 0
    ? Fst
  : V extends [ infer F extends (Sexpr|Each)
              , ...infer Rest extends (Sexpr|Each)[]]
    ? [ RegenFn<F, InsertP>
      , SomeThreadGeneral<Fst, Rest, InsertP>] extends [infer Reged, infer Cont]
      ? ['if', [['sym', 'nil?'], Cont]
          , TNil
          , [Reged, Cont]]
    : Error.ErrorCase<Error.SomeThreadFirstError2, 'insert error', [Fst, V]>
  : Error.ErrorCase<Error.SomeThreadFirstError1, '', [Fst, V]>

export type LispSomeThreadGeneral<
  S
, Flag extends 'second' | 'last'> =
  S extends [infer H extends (Sexpr|Each), ...infer T extends (Sexpr|Each)[]]
    ? T['length'] extends 0
      ? S
    : Reverse<T> extends infer Rev extends (Sexpr|Each)[]
      ? Rev extends unknown[]
        ? SomeThreadGeneral<H, Rev, Flag>
      : Error.ErrorCase<Error.LispSomeThreadGeneralError1, 'reverse error', Rev>
    : never
  : Error.ErrorCase<Error.LispSomeThreadGeneralError0, '-> should have 1 elem', S>

export type LispSomeThreadFirst<S> = LispSomeThreadGeneral<S, 'second'>
export type LispSomeThreadLast<S> = LispSomeThreadGeneral<S, 'last'>


// ---------------------------------------
// -- Eval
// ---------------------------------------

export type BuiltinsUnion =
'if' | 'let' | 'fn' | '->' | '->>' | 'some->' | 'some->>' | 'str' | 'vector' | 'map' | 'filter' | 'remove' | 'reduce' | 'count' | 'concat' | 'conj' | 'join' | 'first' | 'second' | 'third' | 'last' | 'rest' | 'butlast' | 'reverse' | 'repeat' | 'range' | 'interleave' | 'take' | 'drop' | 'assoc-in' | 'update-in' | 'assoc' | 'update' | 'get' | 'get-in' | 'keys' | 'eq' | '=' | 'not' | 'and' | 'or' | 'inc' | 'dec' | '+' | '-' | '*' | '/' | 'trunc' | 'floor' |'%' | 'rem' | 'mod' | 'abs' | '>' | '<' | '>=' | '<=' | 'number?' | 'string?' | 'vector?' | 'map?' | 'fn?' | 'keyword?' | 'ifn?' | 'pos-int?' | 'neg-int?' | 'pos?' | 'neg?' | 'int?' | 'nat?' | 'odd?' | 'even?' | 'zero?' | 'empty?' | 'every?' | 'ratio?' | 'some' | 'nil?' | 'some?' | 'boolean?' | 'any?' | 'prim?' | 'type' | 're-find' | 'split' | 'replace' | 'subs-all' | 'subs' | 'min' | 'max' | 'zipmap' | 'apply'
// deleted: symbol? -> not used in current.
export type BuiltinsFn = Exclude<BuiltinsUnion, 'if' | 'let' | 'fn' | '->' | '->>' | 'some->' | 'some->>'>

type Builtins<
  U
, OPR extends unknown[]
, env
, prev> =
  Reading<OPR, env, [[prev]]> extends infer R
    ? U extends '->'
      ? Eval<LispThreadFirst<OPR>, env, [[prev]]>
    : U extends '->>'
      ? Eval<LispThreadLast<OPR>, env, [[prev]]>
    : U extends 'some->'
      ? Eval<LispSomeThreadFirst<OPR>, env, [[prev]]>
    : U extends 'some->>'
      ? Eval<LispSomeThreadLast<OPR>, env, [[prev]]>
    : U extends `str`
      ? Str<R>
    : U extends `re-find`
      ? LispRefind<R>
    : U extends `split`
      ? LispSplit<R>
    : U extends `subs-all`
      ? LispStrSubsAll<R>
    : U extends `subs`
      ? LispCljSubs<R>
    : U extends `replace`
      ? LispReplace<R>
    : U extends `vector`
      ? LispVector<R>
    : U extends `map`
      ? LispMap<R>
    : U extends `filter`
      ? LispFilter<R>
    : U extends `remove`
      ? LispRemove<R>
    : U extends `reduce`
      ? LispReduce<R>
    : U extends `count`
      ? LispCount<R>
    : U extends `concat`
      ? LispConcat<R>
    : U extends `conj`
      ? LispConj<R>
    : U extends `join`
      ? LispJoin<R>
    : U extends `first`
      ? LispFirst<R>
    : U extends `second`
      ? LispSecond<R>
    : U extends `third`
      ? LispThird<R>
    : U extends `last`
      ? LispLast<R>
    : U extends `rest`
      ? LispRest<R>
    : U extends `butlast`
      ? LispButlast<R>
    : U extends `reverse`
      ? LispReverse<R>
    : U extends `repeat`
      ? LispRepeat<R>
    : U extends 'range'
      ? LispRange<R>
    : U extends `interleave`
      ? LispInterleave<R>
    : U extends `take`
      ? LispTake<R>
    : U extends `drop`
      ? LispDrop<R>
    : U extends `assoc-in`
      ? LispAssocIn<R>
    : U extends `update-in`
      ? LispUpdateIn<R>
    : U extends `assoc`
      ? LispAssoc<R>
    : U extends `update`
      ? LispUpdate<R>
    : U extends `get`
      ? LispGet<R>
    : U extends `get-in`
      ? LispGetIn<R>
    : U extends `keys`
      ? LispKeys<R>
    : U extends `eq` | `=`
      ? LispEq<R>
    : U extends `not`
      ? LispNot<R>
    : U extends `and`
      ? LispAnd<R>
    : U extends `or`
      ? LispOr<R>
    : U extends `inc`
      ? LispInc<R>
    : U extends `dec`
      ? LispDec<R>
    : U extends `+`
      ? LispAdd<R>
    : U extends `-`
      ? LispSub<R>
    : U extends `*`
      ? LispMul<R>
    : U extends `/`
      ? LispDiv<R>
    : U extends `trunc`
      ? LispTrunc<R>
    : U extends `floor`
      ? LispFloor<R>
    : U extends `rem` | `%`
      ? LispRem<R>
    : U extends `mod`
      ? LispMod<R>
    : U extends `abs`
      ? LispAbs<R>
    : U extends `>` | `<` | `>=` | `<=`
      ? LispRelation<U, R>
    : U extends `number?`
      ? LispIsNumber<R>
    : U extends `string?`
      ? LispIsString<R>
    : U extends `vector?`
      ? LispIsVector<R>
    : U extends `map?`
      ? LispIsMap<R>
    : U extends `fn?`
      ? LispIsFn<R>
    : U extends `keyword?`
      ? LispIsKeyword<R>
    : U extends `ifn?`
      ? LispIsIfn<R>
    : U extends `pos-int?`
      ? LispIsPosInt<R>
    : U extends `neg-int?`
      ? LispIsNegInt<R>
    : U extends `pos?`
      ? LispIsPos<R>
    : U extends `neg?`
      ? LispIsNeg<R>
    : U extends `int?`
      ? LispIsInt<R>
    : U extends `ratio?`
      ? LispIsRatio<R>
    : U extends `nat?`
      ? LispIsNat<R>
    : U extends `odd?`
      ? LispIsOdd<R>
    : U extends `even?`
      ? LispIsEven<R>
    : U extends `zero?`
      ? LispIsZero<R>
    : U extends `empty?`
      ? LispIsEmpty<R>
    : U extends `every?`
      ? LispIsEvery<R>
    : U extends `some`
      ? LispSome<R>
    : U extends `nil?`
      ? LispIsNil<R>
    : U extends `boolean?`
      ? LispIsBoolean<R>
    : U extends `type`
      ? LispType<R>
    : U extends `some?`
      ? LispIsSome<R>
    : U extends `any?`
      ? LispIsAny<R>
    : U extends `prim?`
      ? LispIsPrim<R>
    : U extends `min`
      ? LispMin<R>
    : U extends `max`
      ? LispMax<R>
    : U extends `zipmap`
      ? LispZipmap<R>
    : U extends `apply`
      ? LispApply<R>
    : Eval<[ReadLet<U, env>, OPR[0]], env, [prev]>
  : never

export type Eval<
  A
, env = [[]]
, prev = 0
, Vscope extends boolean = false> =
  A extends Sexpr
    ? A extends [infer OPC, ...infer OPR]
      ? env extends EnvLifo
        ? OPC extends Fn & [`fn`, infer syms, infer D]
          ? Eval<[`let`, Interleave<syms, OPR>, D], env, [prev]>
        : OPC extends IfForm & [`if`, infer IFCond, infer IFT, infer IFF]
          ? Eval<IFCond, env, [[prev]]> extends infer IfResult
            ?  Eval<[If<IfResult, IFT, IFF>, OPR[0]], env, [prev]>
          : never
        : OPC extends Sym & [`sym`, infer U]
          ? ReadLet<U, env> extends TNotMatch
            ? Builtins<U,OPR,env,prev>
          : ReadLet<U, env> extends Fn | Keyword | TMap & infer UU
            ? Eval<[UU, ...OPR], env, [prev]>
          : ReadLet<U, env> extends ['sym', BuiltinsUnion] & infer UU
            ? Eval<[UU, ...OPR], env, [prev]>
          : Error.ErrorCase<Error.EvalError3, `1st arg should be fn/keyword/map.`, A, env>
        : IsKeyMapSexpr<ReadLetRecur<A, env>, env> extends true
          ? IsKeyword<OPC> extends true
            ? LispGet<Reading<[...OPR, OPC], env, [[prev]]>>
          : LispGet<Reading<[OPC, ...OPR], env, [[prev]]>>
        : OPC extends LetForm
          ? Eval<[Eval<OPC, env, [[prev]]>, ...OPR], env, [prev]>
        : OPC extends Sexpr
          ? Eval<[Eval<OPC, env, [[prev]]>, ...OPR], env, [prev]>
        : Error.ErrorCase<Error.EvalError4, `the 1st is not a symbol but it should be.`, A, env>
      : env extends unknown[][]
        ? Error.ErrorCase<Error.EvalError9, `env should be arr of arr.`, A, env>
      : Error.ErrorCase<Error.EvalError6, `env 1st should not be [].`, A, env>
    : Error.ErrorCase<Error.EvalError2, ``, A, env>
  : A extends IfForm & [`if`, infer IFCond, infer IFT, infer IFF]
    ? Eval<If<Eval<IFCond, env, [[prev]]>, IFT, IFF>, env, [prev]>
  : A extends Atom
    ? A extends Prim
      ? A
    : A extends Vector & ['vec', ...infer vr]
      ? vr extends []
        ? Vscope extends true
          ? []
        : VecEmpty
      : vr extends [infer va, ...infer vb]
        ? [...(Vscope extends true ? [] : VecEmpty)
	     , (Eval<va,env,prev,va extends Vector ? false : true>)
	     , ...(Eval<['vec',...vb],env,prev,true> extends infer u ? u extends unknown[] ? u : [] : [])]
      : []
    : A extends Sym & [`sym`, infer SS]
      ? ReadLet<SS, env> extends infer U
        ? U extends Atom
          ? U
        : U extends TNotMatch
          ? A
        : [`prim`, U]
      : Error.ErrorCase<Error.EvalError1, 'sym is not desconstructed well as an inner expression.', A, env>
    : ReadLetRecur<A, env> extends infer a
      ? a
    : never
  : A extends LetForm
    ? A extends [`let`, [Sym[], LetVal[]], Sexpr]
      ? A extends [`let`, [infer letsyms, infer letvals], infer LC]
        ? Eval<[`let`, Interleave<letsyms, letvals>, LC], env, [prev]>
      : Error.ErrorCase<Error.EvalError5, '', A, env>
    : A extends [`let`, [[`sym`, infer LN], infer LV, ...infer LRest], infer LC]
      ? LRest extends [[`sym`, infer LRLN], infer LRLV, ...infer RRest]
        ? Eval<[`let`, [[`sym`, LN], LV], [`let`, [[`sym`, LRLN], LRLV, ...RRest], LC]], env, [prev]>
      : LV extends Prim & [`prim`, infer _]
        ? Eval<LC, Let<LN, LV, env>, [prev]>
      : LV extends Sym & [`sym`, infer LP]
        ? LP extends BuiltinsUnion
          ? Eval<LC, Let<LN, LV, env>, [prev]>
        : Eval<LC, Let<LN, ReadLet<LP, env>, env>, [prev]>
      : LV extends LetForm
        ? Eval<[`let`, [[`sym`, LN], Eval<LV, env, [prev]>], LC], env, [prev]>
      : LV extends Fn
        ? Eval<LC, Let<LN, Reading<[LV], env, [[prev]]> extends infer a extends [Fn] ? a[0] : never, env>, [prev]>
      : LV extends Sexpr | Atom
        ? Eval<LV, env, [[prev]]> extends infer ValueEvaluated
          ? ValueEvaluated extends {error: string}
            ? Error.ErrorCase<Error.EvalError12, 'Invalid binding in let form.', ValueEvaluated, env>
          : Eval<LC, Let<LN, ValueEvaluated, env>, [prev]>
        : never
      : LV extends IfForm
        ? Eval<LC, Let<LN, Eval<LV, env, [[prev]]>, env>, [[prev]]>
      : Error.ErrorCase<Error.EvalError7, '', LV, env>
    : A extends ['let', [], infer Sexpr]
      ? Eval<Sexpr, env, [prev]>
    : Error.ErrorCase<Error.EvalError8, 'this is not proper let-form.', A, env>
  : Error.ErrorCase<Error.EvalError11, `Some of Elem isn't Sexpr.`, A, env>

// ----------------------------
// -- Main
// ----------------------------

export namespace Cion {
  export type RawLisp<S extends string> = Eval<Compiler.SCompiler<Compiler.Tokenizer<S>>>
  export type Lisp<S extends string> = Compiler.Unparse<RawLisp<S>>
  export type CionParser<S extends string> = Compiler.Tokenizer<S>
  export type Builtins = BuiltinsUnion
}

export default Cion
