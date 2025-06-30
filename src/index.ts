import type * as Bit from './bit.ts'
import type * as Compiler from './compiler'
import type * as Util from './util'
import type { regex } from './regex'
import type * as ratio from './ratio'

import type {LetVal,LetArg,LetForm,Each,Atom,TMap,Sexpr,TNil,Keyword,Sym,PrimString,PrimBoolean,PrimTestNumber,PrimNumber,BitString,RatioString,NumString,Prim,Args,Fn,IFn,Vector,Var,Env,TNotMatch,IfForm} from './sexprtypes'
import {VNil,VNotMatch} from './sexprtypes'

// -----------------
// -- Error Handle
// -----------------

// If sexpr spits error out, it saves the errors to pass it as a result.
type ErrorMatch = {error: string, message: string, sexpr: unknown}
type ErrorCase<
  Case extends string
, Msg extends string
, S
, Env = []> =
{error: Case, message: Msg, sexpr: S} & (Env extends [] ? {} : {env: Env})

// ---------------
// -- Inner Env
// ---------------

export type MakeVar<N, V> = { name: N; value: V };

type GetVar<
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

// test
const getVarTest: GetVar<
  "s",
  [MakeVar<"ss", "stringer">, MakeVar<"s", "string">]
> = "string";
const getVarTest2: GetVar<
  "ss",
  [MakeVar<"ss", "stringer">, MakeVar<"s", "string">]
> = "stringer";
const EvalTest3: GetVar<
  "sss",
  [MakeVar<"ss", "stringer">, MakeVar<"s", "string">]
> = VNotMatch;

type EnvLifo = Env[];

type LetError0 = "LetError";
type Let<
  N
, V
, EnvLifo = Env[]> =
  EnvLifo extends Env[]
    ? [...EnvLifo, [MakeVar<N, V>]]
  : LetError0

type ReadLetError0 = "ReadLetError0"
type ReadLet<
  N
, EnvLifo = [[]]> =
  EnvLifo extends [...infer HS, infer L]
    ? L extends Env
      ? GetVar<N, L> extends TNotMatch
        ? ReadLet<N, HS>
      : GetVar<N, L>
    : TNotMatch
  : TNotMatch

type LetEnvLifo =
[
  [
    MakeVar<"ss", "stringer">,
    MakeVar<"s", "string">,
    MakeVar<"cc", [`prim`, "p/cc"]>,
  ],
]
const letTest: Let<"sss", "str", LetEnvLifo> = [
  [
    { name: "ss", value: "stringer" },
    { name: "s", value: "string" },
    { name: "cc", value: [`prim`, `p/cc`] },
  ],
  [{ name: "sss", value: "str" }],
];

const readLetTest: ReadLet<"s", Let<"sss", "str", LetEnvLifo>> = "string";
const readLetTest2: ReadLet<"sss", Let<"sss", "str", LetEnvLifo>> = "str";
const readLetTest3: ReadLet<"ssss", Let<"sss", "str", LetEnvLifo>> = VNotMatch;
// test / primitive - case
const readLetTest4: ReadLet<
  "sss",
  Let<"sss", [`prim`, `p/sss`], LetEnvLifo>
> = [`prim`, `p/sss`];
const readLetTest5: ReadLet<"cc", Let<"sss", "str", LetEnvLifo>> = [
  `prim`,
  `p/cc`,
];

type ReadLetRecur<
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

const readletrecur_test0: ReadLetRecur<['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]], Let<'b', ['prim', '0010'], Let<'a', ['prim', '0001'], LetEnvLifo>>> = ['fn', [['sym', 'x']], [['sym', '+'], ['prim', '0001'], ['prim', '0010']]]

type ReadAtom<
  A
, EnvLifo = [[]]
, prev = 0> =
  A extends [`sym`, infer S]
    ? ReadLet<S, EnvLifo>
  : Eval<A, EnvLifo, [prev]>
// test readatom
const readatomtest: ReadAtom<
  [`sym`, `sss`],
  Let<"sss", [`prim`, `p/sss`], LetEnvLifo>
> = [`prim`, `p/sss`];
const readatomtest2: ReadAtom<
  [`prim`, `'sss'`],
  Let<"sss", [`prim`, `p/sss`], LetEnvLifo>
> = [`prim`, `'sss'`];

type ReadingError0 = "ReadingError0";
type ReadingError1 = "ReadingError1";
type ReadingError2 = "ReadingError2";
type ReadingError3 = "ReadingError3";

type Reading<
  AS
, EnvLifo = [[]]
, prev = 0
, R = []> =
  R extends Atom[]
    ? AS extends [infer H, ...infer T]
      ? H extends Sym & ['sym', infer _ extends BuiltinsUnion]
        ? Reading<T, EnvLifo, prev, [...R, H]>
      : H extends Atom
        ? Reading<T, EnvLifo, prev, [...R, ReadAtom<H, EnvLifo, prev>]>
      : H extends Sexpr | LetForm | IfForm
        ? Reading<T, EnvLifo, prev, [...R, Eval<H, EnvLifo, prev>]>
      : ErrorCase<ReadingError1, "", AS>
    : R
  : ErrorCase<ReadingError0, 'sexpr is not atom list.', R>

const readingtest0: Reading<
  [[`sym`, `a`], [`sym`, `b`], [`prim`, `c-str`]],
  [[], [MakeVar<"a", [`prim`, "a-str"]>, MakeVar<"b", [`prim`, "b-str"]>]]
> = [
  ["prim", "a-str"],
  ["prim", "b-str"],
  ["prim", "c-str"],
];
const readingtest1: Reading<
  [['sym', 'a']],
  [[]]
> =
  { sexpr: ["NotMatch"]
    , error: 'ReadingError0'
    , message: 'sexpr is not atom list.'
  }
const readingtest2: Reading<
[['sym', 'a'], ['sym', 'b'], [['sym', 'str'], ['prim', "'s1'"], ['prim', "'s2'"]]],
[[],
 [MakeVar<"a", ['sym', 'str']>, 
  MakeVar<'b', ['prim', "'bs'"]>]]> = [['sym', 'str'], ['prim', "'bs'"], ['prim', "'s1s2'"]]

// -----------------
// -- String Fn
// -----------------

type StrError0 = "StrError0"
export type Str<
  S
, R extends string = ""> =
  S extends [[`prim`, `${infer HS}`], ...infer T]
    ? HS extends `'${infer hs}'` | `'${infer hs}'`
      ? Str<T, `${R}${hs}`>
    : Str<T, `${R}${HS}`>
  : [`prim`, `'${R}'`]

type LispRefindError0 = "LispRefindError0"
export type LispRefind<
  S> =
  S extends [[`prim`, `'${infer regex}'`], [`prim`, `'${infer searched}'`]]
    ? regex.RegexFind<searched, regex> extends [infer _, infer match extends string, infer _]
      ? ['prim', `'${match}'`]
    : ['prim', `''`]
  : S // LispRefindError0



// --------------------------------------------
// -- Logical Operators
// --------------------------------------------

type _And<Fst, Snd> = Fst extends false ? false : Snd extends false ? false : true
type _LispAnd<
  S> =
  S extends [infer Fst, ...infer Rest]
    ? Fst extends [`prim`, infer Boolean]
      ? Boolean extends `nil`
        ? false
      : Rest extends []
        ? Boolean
      : _And<Boolean, _LispAnd<Rest>>
    : never
  : never

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

export type LispOr<
  S> =
  S extends [infer _, ...infer __]
    ? [`prim`, _LispOr<S>]
  : [`prim`, false]

type _Eq<Fst, Snd> = Fst extends Snd ? (Snd extends Fst ? Fst : never) : never;
const lispeqtest: _Eq<[`prim`, "'a'"], [`prim`, "'a'"]> = [`prim`, "'a'"];

type _LispEq<
  S> =
  S extends [infer Fst, ...infer Rest]
    ? Rest extends []
      ? Fst
    : _Eq<Fst, _LispEq<Rest>>
  : never

type LispEqError0 = typeof LispEqError0
const LispEqError0 = "LispEqError0"
export type LispEq<
  S> =
  S extends [infer Fst, ...infer _]
    ? [`prim`, Util.Equal<Fst, _LispEq<S>>]
  : ErrorCase<LispEqError0, "", S>

type _Not<B> = B extends false ? true : false
export type LispNot<S> = S extends [['prim', infer U]] ? ['prim', _Not<U>] : never

// -------------------------------
// -- Bit Operators
// -------------------------------

type LispAddError0 = 'LispAddError0'
type LispAddError1 = 'LispAddError1'
export type LispAdd<
  S
, R extends NumString = "00000000"> =
  S extends []
    ? [`prim`, ratio.ForceRatio<R>]
  : S extends [infer Fst, ...infer Rest]
    ? Fst extends [`prim`, infer FstP extends NumString]
      ? LispAdd<Rest, ratio.Add<ratio.ForceRatio<R>, ratio.ForceRatio<FstP>>>
    : ErrorCase<LispAddError0, "", S>
  : ErrorCase<LispAddError1, "", S>

type LispSubError0 = 'LispSubError0'
export type LispSub<
  S
, R extends NumString = "00000000"
, Init extends boolean = true> =
  S extends []
    ? [`prim`, ratio.ForceRatio<R>]
  : S extends [[`prim`, infer Fst extends NumString], ...infer Rest extends ['prim', NumString][]]
    ? Init extends true
      ? LispSub<Rest, Fst, false>
    : LispSub<Rest, ratio.Sub<ratio.ForceRatio<R>, ratio.ForceRatio<Fst>>, false>
  : ErrorCase<LispSubError0, "", S>

type LispIncError0 = 'LispIncError0'
export type LispInc<
  S> =
  S extends [['prim', infer Fst extends NumString]]
    ? LispAdd<[...S, ['prim', '0000000000000001']]>
  : ErrorCase<LispIncError0, "", S>

type LispDecError0 = 'LispDecError0'
export type LispDec<
  S> =
  S extends [['prim', infer Fst extends NumString]]
    ? LispSub<[...S, ['prim', '0000000000000001']]>
  : ErrorCase<LispDecError0, "", S>

type LispMulError0 = 'LispMulError0'
export type LispMul<
  S
, R extends NumString = "00000000"
, Init extends boolean = true> =
  S extends []
    ? [`prim`, ratio.ForceRatio<R>]
  : S extends [[`prim`, infer Fst extends NumString], ...infer Rest extends ['prim', NumString][]]
    ? Init extends true
      ? LispMul<Rest, Fst, false>
    : LispMul<Rest, ratio.Mul<ratio.ForceRatio<R>, ratio.ForceRatio<Fst>>, false>
  : ErrorCase<LispMulError0, "", S>

type LispDivError0 = 'LispDivError0'
type LispDivError1 = 'LispDivError1'
export type LispDiv<
  S
, R extends NumString = "00000001"
, Init extends boolean = true> =
  S extends []
    ? [`prim`, ratio.ForceRatio<R>]
  : S extends [[`prim`, infer Fst extends NumString], ...infer Rest extends ['prim', NumString][]]
    ? Init extends true
      ? LispDiv<Rest, Fst, false>
    : ratio.Div<ratio.ForceRatio<R>, ratio.ForceRatio<Fst>> extends infer Div
      ? Div extends 'nil'
        ? Bit.Nil
      : Div extends NumString
        ? LispDiv<Rest, Div, false>
      : never
    : ErrorCase<LispDivError0, "", S>
  : ErrorCase<LispDivError1, "", S>



type LispModError0 = 'LispModError0'
type LispModError1 = 'LispModError1'
export type LispMod<
  S
, R extends NumString = "0000000000000000"
, Init extends boolean = true> =
  S extends []
    ? [`prim`, R]
  : S extends [ [`prim`, infer Fst extends NumString]
              , ...infer Rest extends ['prim', NumString][]]
    ? Init extends true
      ? Bit.BitGTE<ratio.ForceNat<Fst> extends infer f extends string?f:never, '0000000000000000'> extends true
        ? LispMod<Rest, Fst, false>
      : Rest extends [['prim', infer Snd extends NumString]]
        ? ratio.Scaling<ratio.ForceRatio<Fst>, ratio.ForceRatio<Snd>> extends [[infer rfst extends BitString, infer _rfstm], [infer rsnd extends BitString, infer _rsndm]]
          ? LispMod<[['prim', Bit.BitSub<rsnd, Bit.BitRevSign<rfst>>], ['prim', Snd]]>
        : never
      : never
    : ratio.Scaling<ratio.ForceRatio<Fst>, ratio.ForceRatio<R>> extends [[infer rfst extends BitString, infer _rfstm], [infer rsnd extends BitString, infer _rsndm]]
      ? Bit.BitMod<rsnd,rfst> extends Bit.Nil | string & infer Mod
        ? Mod extends string
          ? LispMod<Rest, Mod, false>
        : Bit.Nil
      : never
    : ErrorCase<LispModError0, '', S>
  : ErrorCase<LispModError1, '', S>

type LispRelationError0 = 'LispRelationError0'
type LispRelationError1 = 'LispRelationError1'
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
      ? LispRelation<Name, Rest, Fst, false, ratio.Relation<ratio.ForceRatio<R>, ratio.ForceRatio<Fst>, Name>>
    : [`prim`, false]
  : ErrorCase<LispRelationError1, "", S>

export type Eq<L, R> = Util.Equal<L,R>
type If<A, B, C> = A extends [`prim`, false] | TNil ? C : B;

// predicate
// - number?, string?, vector?, map?, fn?, ifn?, pos-int?, neg-int?, odd?, even?, zero?, symbol?, keyword?,  empty? 
type NatNumber = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'
type OddNumber = '1'|'3'|'5'|'7'|'9'
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

type IsOdd<
  S extends string> =
  true extends IsNumber<S>
    ? _IsOdd<S>
  : false

type IsEven<
  S extends string> =
  true extends IsNumber<S>
    ? true extends _IsOdd<S>
      ? false
    : true
  : false

// number?
export type LispIsNumber<
  S> =
  S extends [['prim', infer N extends BitString]]
    ? N extends `1${infer rN}`
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

// ifn?
export type LispIsIfn<
  S> =
  LispIsKeyword<S> extends ['prim', false]
    ? LispIsFn<S>
  : ['prim', true]

// pos-int?
export type LispIsPosInt<
  S> =
  S extends [['prim', infer N extends BitString]]
    ? N extends `1${infer _}`
      ? ['prim', false]
    : LispIsNumber<S>
  : S extends [['prim', infer N extends RatioString]]
    ? ratio.ForceNat<N> extends infer D extends BitString
      ? D extends 'nil'
        ? ['prim', false]
      : D extends `1${infer _}`
        ? ['prim', false]
      : LispIsNumber<[['prim', D]]>
    : never
  : ['prim', false]

// neg-int?
export type LispIsNegInt<
  S> =
  S extends [['prim', infer N extends BitString]]
    ? N extends `1${infer _}`
      ? LispIsNumber<S>
    : ['prim', false]
  : S extends [['prim', infer N extends RatioString]]
    ? ratio.ForceNat<N> extends infer D extends BitString
      ? D extends 'nil'
        ? ['prim', false]
      : D extends `1${infer _}`
        ? LispIsNumber<[['prim', D]]>
      : ['prim', false]
    : never
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
    ? ratio.ForceNat<N> extends infer D extends string
      ? D extends 'nil'
        ? ['prim', false]
      : ['prim', IsEven<D>]
    : never
  : ['prim', false]

// zero?
export type LispIsZero<
  S> =
  S extends [['prim', infer N extends string]]
    ? Bit.BitIsZero<N> extends true
      ? N extends 'nil'
        ? ['prim', false]
      : ['prim', true]
    : ['prim', false]
  : S extends [['prim', infer N extends RatioString]]
    ? ratio.ForceNat<N> extends infer D extends string
      ? D extends 'nil'
        ? ['prim', false]
      : Bit.BitIsZero<D> extends true
        ? ['prim', true]
      : ['prim', false]
    : never
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
  : ['prim', false]

export type LispIsBoolean<
  S> =
  S extends [['prim', true]] | [['prim', false]]
    ? ['prim', true]
  : ['prim', false]

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

type ConcatError0 = "ConcatError0"
type ConcatError1 = "ConcatError1"
type ConcatError2 = "ConcatError2"
type TConcat<
  V extends Array<Array<unknown>>
, Stack extends Array<unknown> = []> =
  V['length'] extends 0
    ? Stack
  : V extends [infer Head extends Array<unknown>, ...infer Rest extends Array<Array<unknown>>]
    ? TConcat<Rest, [...Stack, ...Head]>
  : never

type VConsError0 = 'VConsError0'
type VConsError1 = 'VConsError1'
type VConsError2 = 'VConsError2'
type VCons<
  V
, R extends unknown[] = []> =
  V extends ['vec', infer v, infer vv]
    ? VCons<vv, [...R, v]>
  : V extends ['vec', infer v]
    ? VCons<['vec'], [...R, v]>
  : V extends ['vec']
    ? ['vec', ...R]
  : ['vec', ...R, V]
        
const testvcons0: VCons<['vec', 1, ['vec', 2, ['vec', 3]]]> = ['vec', 1, 2, 3]        
const testvcons1: VCons<['vec', 1, ['vec', 2, ['vec', 3, ['vec']]]]> = ['vec', 1, 2, 3]
const testvcons2: VCons<['vec', 1, ['vec', 2, ['vec', 3, 3]]]> = ['vec', 1, 2, 3, 3]

export type LispConcat<
  S
, R extends unknown[][] = []> =
  S extends Vector[] & [['vec', ...infer H], ...infer T]
    ? T extends []
      ? ['vec', ...TConcat<[...R, H]>]
    : LispConcat<T, [...R, H]>
  : ErrorCase<ConcatError0, '', S>

const tconcattest0: TConcat<[[0,1], [2,3], [4,5]]> = [0,1,2,3,4,5]

const ttm: TMap = ['map', [['key', ':b'], ['key', ':b']]]

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

type GetMapError0 = "GetMapError0";
type GetMapError1 = "GetMapError1";
type GetMapError2 = "GetMapError2";

type GetMap<
  K
, V
, sV = V extends [infer _, infer i] ? i : never> =
  sV extends [infer k, infer v, ... infer _]
    ? k extends K
      ? v
    : GetMap<K,V,sV extends [infer _, infer __, ...infer i] ? i : never>
  : TNil

const testgetmap0: GetMap<['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]> = ['prim', '0']
const testgetmap1: GetMap<['key', ':a'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]> = ['prim', '0']
const testgetmap2: GetMap<['key', ':c'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]> = VNil

type GetVecError0 = 'GetVecError0'
type GetVecError1 = 'GetVecError1'
type GetVecError2 = 'GetVecError2'
type GetVec<
  Idx extends PrimNumber
, Vec extends Vector> =
  Vec extends ['vec', infer H, ...infer T extends Atom[]]
    ? Idx extends PrimNumber & ['prim', infer idx extends string]
      ? Bit.BitIsZero<idx> extends true
        ? H
      : Bit.BitGT<idx, "0"> extends true
        ? T extends []
          ? TNil
        : GetVec<['prim', Bit.BitSub<idx, "1">], ['vec', ...T]>
      : ErrorCase<GetVecError0, '', [Idx, Vec]>
    : ErrorCase<GetVecError1, '', [Idx, Vec]>
  : TNil

type GetError0 = 'GetError0'
type GetError1 = 'GetError1'
type GetError2 = 'GetError2'
export type Get<
  K extends PrimNumber | Keyword
, V extends Vector | TMap> =
  V extends Vector
    ? K extends PrimNumber
      ? GetVec<K, V>
    : ErrorCase<GetError0, '', [K,V]>
  : GetMap<K, V>

type LispGetError0 = "LispGetError0"
export type LispGet<
  S> =
  S extends [infer Map extends TMap, infer Key extends Keyword]
    ? Get<Key, Map>
  : S extends [infer Vec extends Vector, infer Idx extends ['prim', BitString]]
    ? Get<Idx, Vec>
  : S extends [infer Vec extends Vector, infer Idx extends ['prim', NumString]]
    ? ratio.ForceNat<Idx[1]> extends infer D extends string
      ? D extends 'nil'
        ? TNil
      : Get<D extends PrimNumber ? D : never, Vec>
    : never
  : S extends [infer Map extends TMap, infer Idx extends PrimNumber]
    ? TNil
  : S extends [infer MNil extends TNil | ['vec'] | ['map'], infer _]
    ? TNil
  : ErrorCase<LispGetError0, "this is not map and key or vector and idx-num.", S>

type LispGetInError0 = 'LispGetInError0'
type LispGetInError1 = 'LispGetInError1'
export type LispGetIn<
  S> =
  S extends [infer f, infer s]
    ? s extends ['vec', infer j, ...infer k]
      ? k extends []
        ? LispGet<[f, j]>
      : LispGetIn<[LispGet<[f, j]>, ['vec', ...k]]>
    : ErrorCase<LispGetInError1, 'the second should be a vector.', S>
  : ErrorCase<LispGetInError0, '', S>

type LispSecondError0 = "LispSecondError0"
export type LispSecond<
  S> =
  S extends [Vector]
    ? LispGet<[...S, ['prim', '0000000000000001']]>
  : ErrorCase<LispSecondError0, "arg should be a vector.", S>

type AssocError0 = 'AssocError0'
type AssocError1 = 'AssocError1'
type AssocError2 = 'AssocError2'
type AssocError3 = 'AssocError3'
const AssocError0 = 'Args: 1st Map & 2nd not keyword.'
const AssocError1 = 'Args: 1st Vector & 2nd not number.'
const AssocError2 = 'AssocError2'
const AssocError3 = 'AssocError3'
type AssocErrorMsg0 = 'Args: 1st Map & 2nd not keyword.'
type AssocErrorMsg1 = 'Args: 1st Vector & 2nd not number.'
export const AssocErrorMsg1 = 'Args: 1st Vector & 2nd not number.'

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
      : Bit.BitGT<kB, '0'> extends true
        ? mR extends []
          ? ['vec', ...S, mV]
        : _Assoc<['vec', ...mR], ['prim', Bit.BitSub<kB, '1'>], V, Type, [...S, mV]>
      : ['vec', ...S, V, ...mR]
    : ErrorCase<AssocError1, AssocErrorMsg1, M>
  : M extends TMap & ['map', [infer mK extends Keyword, infer mV extends Atom, ...infer mR]]
    ? mK extends K
      ? ['map', [...S, mK, Type extends 'update' ? Eval<[V, mV]> : V, ...mR]]
    : mR extends []
      ? ['map', [...S, mK, mV, K, V]]
    : _Assoc<['map', mR], K, V, Type, [...S, mK, mV]>
  : ErrorCase<AssocError0, AssocErrorMsg0, M>

type AssocInError0 = 'AssocInError0'
type AssocInError1 = 'AssocInError1'
type AssocInError2 = 'AssocInError2'
type AssocInError3 = 'AssocInError3'
type AssocInError4 = 'AssocInError4'
type AssocInError5 = 'AssocInError5'
type AssocInError6 = 'AssocInError6'
type AssocInError7 = 'AssocInError7'
type AssocInError8 = 'AssocInError8'

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
        ? Recur extends Atom
          ? _Assoc<M, Kh, Recur>
        : ErrorCase<AssocInError7, `The value of key (${Kt[0][1] extends RatioString ? `${Kt[0][1][0]}/${Kt[0][1][1]}` : Kt[0][1] extends string ? Kt[0][1] : never}) is not vector nor map.`, M>
      : ErrorCase<AssocInError3, '', M>
    : ErrorCase<AssocInError8, "Keys rests but its value is not vector nor map.", M>
  : ErrorCase<AssocInError4, "", M>
 
// [note]
// - This accepts only a keyword if the element is map, Clojure can take it though.
export type _AssocIn<
  M extends Vector | TMap
, Ks extends ['vec', ...(Keyword | PrimNumber | ['prim', RatioString])[]]
, V extends Atom
, Type extends 'update' | 'assoc' = 'assoc'> =
  M extends Vector
    ? M extends ['vec']
      ? M
    : Ks extends ['vec', infer Kh extends ['prim', RatioString], ...infer Kt extends (Keyword | PrimNumber | ['prim', RatioString])[]]
      ? ratio.ForceNat<Kh[0]> extends infer D extends string
        ? D extends 'nil'
          ? TNil
        : _rAssocIn<M, ['prim', D], Kt, V, Type>
      : never
    : Ks extends ['vec', infer Kh extends PrimNumber, ...infer Kt extends (Keyword | PrimNumber | ['prim', RatioString])[]]
      ? _rAssocIn<M, Kh, Kt, V, Type>
    : AssocInError0
  : M extends TMap
    ? Ks extends ['vec', infer Kh extends Keyword, ...infer Kt extends (Keyword | PrimNumber | ['prim', RatioString])[]]
      ? _rAssocIn<M, Kh, Kt, V, Type>
    : AssocInError5
  : AssocInError6

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

type LispAssocError0 = 'LispAssocError0'
type LispAUErrorMsg  = '1st or 2nd is not proper form.'
export type LispAssoc<
  S> =
  S extends [ infer M extends Vector | TMap
  , infer K
  , infer V extends Atom]
    ? K extends ['prim', RatioString]
      ? ratio.ForceNat<K[0]> extends infer D extends string
        ? D extends 'nil'
          ? TNil
        : _Assoc<M,['prim', D],V>
      : never
    : K extends Keyword | PrimNumber
      ? _Assoc<M,K,V>
    : never
  : ErrorCase<LispAssocError0, LispAUErrorMsg, S>

type LispAssocInError0 = 'LispAssocInError0'
export type LispAssocIn<
  S> =
  S extends [ infer M extends Vector | TMap
  , infer Ks extends ['vec', ...(Keyword | PrimNumber | ['prim', RatioString])[]]
  , infer V extends Atom]
    ? _AssocIn<M,Ks,V>
  : ErrorCase<LispAssocInError0, LispAUErrorMsg, S>

type LispUpdateError0 = 'LispUpdateError0'
export type LispUpdate<
  S> =
  S extends [ infer M extends Vector | TMap
  , infer K extends Keyword | PrimNumber | ['prim', RatioString]
  , infer V extends Fn | ['sym', BuiltinsFn]]
    ? _Update<M,K,V>
  : ErrorCase<LispUpdateError0, LispAUErrorMsg, S>
type LispUpdateInError0 = 'LispUpdateInError0'
type LispUpdateInError1 = 'LispUpdateInError1'
type LispUpdateInError2 = 'LispUpdateInError2'
type LispUpdateInError3 = 'LispUpdateInError3'
export type LispUpdateIn<
  S> =
  S extends [ infer M extends Vector | TMap
  , infer Ks extends ['vec', ...(Keyword | PrimNumber | ['prim', RatioString])[]]
  , infer V extends Fn | ['sym', BuiltinsFn]]
    ? _UpdateIn<M,Ks,V>
  : S extends [infer M, infer Ks, infer _V]
    ? M extends Vector | TMap
      ? Ks extends ['vec', ...(Keyword | PrimNumber | ['prim', RatioString])[]]
        ? ErrorCase<LispUpdateInError3, "The 3rd must be fn.", S>
      : ErrorCase<LispUpdateInError2, "The 2st must be key vec.", S>
    : ErrorCase<LispUpdateInError1, "The 1st must be vec or map.", S>
  : ErrorCase<LispUpdateInError0, "", S>

type LispVectorError0 = "LispVectorError0"
export type LispVector<S> = S extends unknown[] ? ['vec', ...S] : ErrorCase<LispVectorError0, `Sexpr's inner expression is not array.`, S>

type CountError0 = "CountError0"
type CountError1 = "CountError1"
export type Count<
  S extends unknown[]
, I extends string = '0'> =
  S extends [infer _, ...infer R]
    ? R extends []
      ? Bit.BitAdd<I,'1'>
    : Count<R, Bit.BitAdd<I, '1'>>
  : ErrorCase<CountError0, '1st should be an array as an inner expression.', S>
export type LispCount<
  S> =
  S extends [Vector] & [['vec']]
    ? ['prim', '0']
  : S extends [Vector] & [['vec', ...infer V]]
    ? ['prim', Count<V>]
  : ErrorCase<CountError1, 'Arg of count should be vector.', S>

// ------------
// -- getter
// ------------

type FirstError0 = "FirstError0";
type FirstError1 = "FirstError1";
type RestError0 = "RestError0";
type RestError1 = "RestError1"
type CommonArgVecErrMsg = 'arg should be vector as an inner expression.'
type CommonArgVecErrMsgFn<S extends string> = `arg of ${S} should be a vector.`

type ConcatError = "ConcatError";

export type First<
  V> =
  V extends Vector & [`vec`, infer H, ...infer _]
    ? H
  : V extends ['vec']
    ? TNil
  : ErrorCase<FirstError0, CommonArgVecErrMsg, V>
export type LispFirst<S> = S extends [infer V extends Vector] ? First<V> : ErrorCase<FirstError1, CommonArgVecErrMsgFn<'first'>, S>

type LastError0 = 'LastError0'
type LastError1 = 'LastError1'
type Last<
  V> =
  V extends [infer H, ...infer T]
    ? T extends []
      ? H
    : Last<T>
  : ErrorCase<LastError0, CommonArgVecErrMsg, V>
export type LispLast<S> = S extends [['vec']] ? TNil : S extends [['vec', ...infer V]] ? Last<V> : ErrorCase<LastError1, CommonArgVecErrMsgFn<'last'>, S>

export type Rest<
  V> =
  V extends Vector & [`vec`, infer _, ...infer T]
    ? T[0] extends Atom
      ? [`vec`, ...T]
    : [`vec`]
  : ErrorCase<RestError0, CommonArgVecErrMsg, V>
export type LispRest<
  S> =
  S extends [infer V extends Vector]
    ? Rest<V>
  : ErrorCase<RestError1, CommonArgVecErrMsgFn<'rest'>, S>

type ButlastError0 = "ButlastError0"
type ButlastError1 = "ButlastError1"
type ButlastError2 = "ButlastError2"
type ButlastError3 = "ButlastError3"
type _Butlast<
  V
, R extends unknown[] = []> =
  V extends [infer H, ...infer T]
    ? T extends []
      ? R
    : _Butlast<T,[...R,H]>
  : ErrorCase<ButlastError0, CommonArgVecErrMsg, V>
export type Butlast<V> = V extends Vector & ['vec', ...infer v] ? _Butlast<v> extends Atom[] ? ['vec', ..._Butlast<v>] : ErrorCase<ButlastError1, CommonArgVecErrMsg, V> : ErrorCase<ButlastError3, CommonArgVecErrMsg, V>
export type LispButlast<S> = S extends [infer V extends Vector] ? Butlast<V> : ButlastError2

// -------------
// -- new seq
// -------------

type ConjError0 = "ConjError0"
type ConjError1 = "ConjError1"
type ConjError2 = "ConjError2"
type ConjError3 = "ConjError3"
export type Conj<
  V
, E> =
  E extends Atom
    ? V extends Vector
      ? [...V, E]
    : ErrorCase<ConjError0, '1st should be vector', [V,E]>
  : ErrorCase<ConjError0, '2nd should be Atom', [V,E]>
type LispConj<
  S> =
  S extends [infer H extends Vector, ...infer T extends Atom[]]
    ? [...H, ...T]
  : ErrorCase<ConjError2, CommonArgVecErrMsgFn<'conj'>, S>

export type Concat<
  V
, W> =
  V extends Vector
    ? W extends Vector & [`vec`, ...infer WW]
      ? [...V, ...WW]
    : ErrorCase<ConcatError, '1st should be vector', [V,W]>
  : ErrorCase<ConcatError, '2nd should be vector', [V,W]>

type TakeError0 = "TakeError0"
type TakeError1 = "TakeError1"
type TakeError2 = "TakeError2"
type TakeError3 = "TakeError3"
export type Take<
  N extends NumString
, V extends unknown[]
, R extends unknown[] = []> =
  N extends string
    ? V extends []
      ? R
    : Bit.BitGTE<"0", N> extends true
      ? R
    : V extends [infer F, ...infer T]
      ? Take<Bit.BitSub<N, "1">, T, [...R, F]>
    : ErrorCase<TakeError0, '2nd should be an array.', [N,V,R]>
  : N extends RatioString
    ? ratio.ForceNat<N> extends infer D extends string
      ? D extends 'nil'
        ? TNil
      : Take<D, V, R>
    : never
  : never

type LispTake<
  S> =
  S extends [['prim', infer N extends NumString], ['vec', ...infer V]]
    ? Take<N,V> extends infer RV
      ? RV extends unknown[]
        ? ['vec', ...RV]
      : ErrorCase<TakeError2, 'take spits an inner error', RV>
    : never
  : ErrorCase<TakeError1, '1st and 2nd should be a number and a vector.', S>

type DropError0 = "DropError0"
type DropError1 = "DropError1"
type DropError2 = "DropError2"
type DropError3 = "DropError3"
export type Drop<
  N extends NumString
, V extends unknown[]
, R extends unknown[] = []> =
  N extends string
    ? V extends []
      ? R
    : V extends [infer _, ...infer T]
      ? Bit.BitGTE<"0", N> extends true
        ? V
      : Drop<Bit.BitSub<N, "1">, T>
    : ErrorCase<DropError0, '2nd should be vector', [N,V]>
  : N extends RatioString
    ? ratio.ForceNat<N> extends infer D extends string
      ? D extends 'nil'
        ? TNil
      : Drop<D, V, R>
    : never
  : never

type LispDrop<
  S> =
  S extends [['prim', infer N extends NumString], ['vec', ...infer V]]
    ? Drop<N,V> extends infer RV
      ? RV extends unknown[]
        ? ['vec', ...RV]
      : ErrorCase<DropError2, 'drop spits an inner error', RV>
    : never
  : ErrorCase<DropError1, '1st and 2nd should be a number and a vector.', S>

// -------------------------------------
// -- map, filter, remove, every, some
// -------------------------------------

type FMapError = "MapError";
type FilterError = "FilterError";
type RemoveError = "RemoveError";
type EveryError = "EveryError";
type SomeError = "SomeError";
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
  : ErrorCase<FMapError, 'map should have 2 args', S>

type FilterError0 = "FilterError0"
type FilterError1 = "FilterError1"
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
  : [ErrorCase<FilterError0, "2nd should be vector", [F, V]>]

export type Filter<F, V> = [`vec`, ..._Filter<F, V>];

export type LispFilter<
  S> =
  S extends [infer f, infer vs]
    ? Filter<f, vs>
  : ErrorCase<FilterError1, "filter should have 2 args.", S>

type  RemoveError0 = "RemoveError0"
const RemoveError0 = "RemoveError0"
export type LispRemove<
  S> =
  S extends [infer f, infer vs]
    ? Filter<['fn', [['sym', 'aaa']], [['sym', 'not'], [f, ['sym', 'aaa']]]], vs>
  : ErrorCase<RemoveError0, "remove should have 2 args.", S>

// every?
type EveryError0 = 'EveryError0'
type EveryError1 = 'EveryError1'
export type LispIsEvery<
  S> =
  S extends [infer f, infer vs]
    ? vs extends ['vec']
      ? ['prim', false]
    : Eval<[['sym', '='], [['sym', 'filter'], f, vs], vs]>
  : ErrorCase<EveryError0, 'every should have 2 args.', S>

type SomeError0 = 'SomeError0'
type SomeError1 = 'SomeError1'
export type LispSome<
  S> =
  S extends [infer f, infer vs]
    ? vs extends ['vec']
      ? ['prim', false]
    : Eval<[['sym', '->>'], vs, [['sym', 'filter'], f], ['sym', 'count'], ['sym', 'zero?'], ['sym', 'not']]>
  : ErrorCase<EveryError0, 'every should have 2 args.', S>

type LispIsNilError0 = 'LispIsNilError0'
export type LispIsNil<
  S> =
  S extends [infer A]
    ? A extends TNil
      ? ['prim', true]
    : ['prim', false]
  : ErrorCase<LispIsNilError0, '[compile error] S should be wraped with a taple.', S>

type LispIsSomeError0 = 'LispIsSomeError0'
export type LispIsSome<
  S> =
  S extends [infer A]
    ? A extends TNil
      ? ['prim', false]
    : ['prim', true]
  : ErrorCase<LispIsSomeError0, '[compile error] S should be wraped with a taple.', S>

type InterleaveError0 = "InterleaveError0"
type InterleaveError1 = "InterleaveError1";
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
export type LispInterleave<S> = S extends [['vec', ...infer V], ['vec', ...infer W]] ? ['vec', ...Interleave<V, W>] : ErrorCase<InterleaveError1, 'interleave should have 2 vector.', S> 
type ReduceError0 = 'ReduceError0'
type ReduceError1 = 'ReduceError1'
type ReduceError2 = 'ReduceError2'
type _Reduce<
  F
, Init
, V> =
  V extends [infer H, ...infer T]
    ? T['length'] extends 0
      ? Eval<[F, Init, H]>
    : _Reduce<F, Eval<[F, Init, H]>, T>
  : ErrorCase<ReduceError0, '', [F,Init,V]>
export type Reduce<
  F
, Init
, V> =
  V extends ['vec', ...infer v]
    ? _Reduce<F,Init,v>
  : ErrorCase<ReduceError1, '', [F,Init,V]>
export type LispReduce<
  S> =
  S extends [infer f, infer init, infer v]
    ? Reduce<f,init,v>
  : ErrorCase<ReduceError2, 'reduce should have 3 args.', S>

type ReverseError0 = 'ReverseError0'
type ReverseError1 = 'ReverseError1'
type ReverseError2 = 'ReverseError2'
type ReverseError3 = 'ReverseError3'
export type Reverse<
  V
, R extends Array<unknown> = []> =
  V extends [infer H, ...infer T]
    ? T['length'] extends 0
      ? [H, ...R]
    : Reverse<T, [H, ...R]>
  : V extends []
    ? []
  : { error: [ReverseError0] }

type LispReverse<S> = S extends [Vector] & [['vec', ...infer V]] ? Reverse<V> extends infer RV ? RV extends unknown[] ? ['vec', ...RV] : ErrorCase<ReverseError1, 'reverse error', RV> : never : ErrorCase<ReverseError3, 'reverse should have 1 vector.', S>

// note : for threading macros: insertsecond, insertlast, vecwrap
type InsertSecondError0 = 'InsertSecondError0'
type InsertSecondError1 = 'InsertSecondError1'
type InsertSecond<
  V
, E> =
  V extends [infer H, ...infer R]
    ? [H, E, ...R]
  : V extends [...infer R]
    ? [E, ...R]
  : ErrorCase<InsertSecondError0, '', [V,E]>

const insert2ndtest0: InsertSecond<[0,1,2,3], 'x'> = [0,'x',1,2,3]
const insert2ndtest1: InsertSecond<[0], 'x'> = [0,'x']
const insert2ndtest2: InsertSecond<[], 'x'> = ['x']
const insert2ndtest3: InsertSecond<[], ['a', 'x']> = [['a', 'x']]
const insert2ndtest4: InsertSecond<['b', 'y'], ['a', 'x']> = ['b', ['a', 'x'], 'y']
const insert2ndtest5: InsertSecond<[['b', 'y']], ['a', 'x']> = [['b', 'y'], ['a', 'x']]

type InsertLastError0 = 'InsertLastError0'
type InsertLast<
  V
, E> =
  V extends [...infer R]
    ? [...R, E]
  : ErrorCase<InsertLastError0, '', [V,E]>
const insertlasttest0: InsertLast<[0,1,2,3], 'x'> = [0,1,2,3,'x']
const insertlasttest1: InsertLast<[0], 'x'> = [0,'x']
const insertlasttest2: InsertLast<[], 'x'> = ['x']

type VecWrapError0 = 'VecWrapError0'
// note : any sexpr and any atom of them should be rendered 
//        such as [['sym', 'inc'], ['prim', '0']] and ['prim', '0'].
type VecWrap<V> = V extends unknown[][] ? V : [V]

type ThreadFirstError0 = 'ThreadFirstError0'
type ThreadFirstError1 = 'ThreadFirstError1'
type ThreadFirstError2 = 'ThreadFirstError2'
type ThreadFirstError3 = 'ThreadFirstError3'
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
  : ErrorCase<ThreadFirstError1, '', [Fst, V, R, Init]>

type LispThreadFirstError0 = 'LispThreadFirstError0'
export type LispThreadFirst<
  S> =
  S extends [infer H, ...infer T]
    ? T['length'] extends 0
      ? S
    : ThreadFirst<H,T>
  : ErrorCase<LispThreadFirstError0, '-> should have 1 elem', S>

type ThreadLastError0 = 'ThreadLastError0'
type ThreadLastError1 = 'ThreadLastError1'
type ThreadLastError2 = 'ThreadLastError2'
type ThreadLastError3 = 'ThreadLastError3'
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
  : ErrorCase<ThreadLastError1, '', [Fst, V, R]>

type LispThreadLastError0 = 'LispThreadLastError0'
export type LispThreadLast<
  S> =
  S extends [infer H, ...infer T]
    ? T['length'] extends 0
      ? S
    : ThreadLast<H,T>
  : ErrorCase<LispThreadLastError0, '->> should have 1 elem', S>

type SomeThreadFirstError0 = 'SomeThreadFirstError0'
type SomeThreadFirstError1 = 'SomeThreadFirstError1'
type SomeThreadFirstError2 = 'SomeThreadFirstError2'
type SomeThreadFirstError3 = 'SomeThreadFirstError3'
type TmpGensym = ['sym', 'm']

type SomeLetWrap<
  T> =
['if', T, T, TNil]

export type SomeThreadGeneral<
  Fst
, V extends (Sexpr|Each)[]
, InsertP extends 'second' | 'last'> =
  V['length'] extends 0
    ? Fst
  : V extends [infer Head extends (Sexpr|Each), ...infer Tail extends (Sexpr|Each)[]]
    ? Tail['length'] extends 0
      ? [InsertSecond<VecWrap<Head>, Fst>, InsertP] extends [infer C extends (Sexpr|Each), 'second']
        ? ['if', C, C, ['prim', 'false']]
      : [InsertLast<VecWrap<Head>, Fst>] extends [infer C]
        ? SomeLetWrap<C>
      : never
    : ( InsertP extends 'second'
          ? ThreadFirst<SomeThreadGeneral<Fst, Tail, InsertP>, [Head]>
        : ThreadLast<SomeThreadGeneral<Fst, Tail, InsertP>, [Head]>) extends infer ThreadWrap
      ? ['if', ThreadWrap, ThreadWrap, TNil]
    : ErrorCase<SomeThreadFirstError2, 'insert error', [InsertSecond<VecWrap<Head>, Fst>, V]>
  : ErrorCase<SomeThreadFirstError1, '', [Fst, V]>

type LispSomeThreadGeneralError0 = 'LispSomeThreadGeneralError0'
type LispSomeThreadGeneralError1 = 'LispSomeThreadGeneralError1'
export type LispSomeThreadGeneral<
  S
, Flag extends 'second' | 'last'> =
  S extends [infer H extends (Sexpr|Each), ...infer T extends (Sexpr|Each)[]]
    ? T['length'] extends 0
      ? S
    : Reverse<T> extends infer Rev extends (Sexpr|Each)[]
      ? Rev extends unknown[]
        ? SomeThreadGeneral<H, Rev, Flag>
      : ErrorCase<LispSomeThreadGeneralError1, 'reverse error', Rev>
    : never
  : ErrorCase<LispSomeThreadGeneralError0, '-> should have 1 elem', S>

type LispSomeThreadFirstError0 = 'LispSomeThreadFirstError0'
type LispSomeThreadFirstError1 = 'LispSomeThreadFirstError1'
type LispSomeThreadLastError0 = 'LispSomeThreadLastError0'
type LispSomeThreadLastError1 = 'LispSomeThreadLastError1'
export type LispSomeThreadFirst<S> = LispSomeThreadGeneral<S, 'second'>
export type LispSomeThreadLast<S> = LispSomeThreadGeneral<S, 'last'>


// ---------------------------------------
// -- Eval
// ---------------------------------------

export type BuiltinsUnion =
'->' | '->>' | 'some->' | 'some->>' | 'str' | 'vector' | 'map' | 'filter' | 'remove' | 'reduce' | 'count' | 'concat' | 'conj' | 'first' | 'second' | 'last' | 'rest' | 'butlast' | 'reverse' | 'interleave' | 'take' | 'drop' | 'assoc-in' | 'update-in' | 'assoc' | 'update' | 'get' | 'get-in' | 'eq' | '=' | 'not' | 'and' | 'or' | 'inc' | 'dec' | '+' | '-' | '*' | '/' | '%' | 'mod' | '>' | '<' | '>=' | '<=' | 'number?' | 'string?' | 'vector?' | 'map?' | 'fn?' | 'keyword?' | 'ifn?' | 'pos-int?' | 'neg-int?' | 'odd?' | 'even?' | 'zero?' | 'symbol?' | 'empty?' | 'every?' | 'some' | 'nil?' | 'some?' | 'boolean?' | 'type' | 're-find'
export type BuiltinsFn = Exclude<BuiltinsUnion, 'if' | 'let' | 'fn' | '->' | '->>' | 'some->' | 'some->>'>

type Builtins<
  U
, OPR extends unknown[]
, env
, prev> =
  U extends '->'
    ? Eval<LispThreadFirst<OPR>, env, [[prev]]>
  : U extends '->>'
    ? Eval<LispThreadLast<OPR>, env, [[prev]]>
  : U extends 'some->'
    ? Eval<LispSomeThreadFirst<OPR>, env, [[prev]]>
  : U extends 'some->>'
    ? Eval<LispSomeThreadLast<OPR>, env, [[prev]]>
  : U extends `str`
    ? Str<Reading<OPR, env, [[prev]]>>
  : U extends `re-find`
    ? LispRefind<Reading<OPR, env, [[prev]]>>
  : U extends `vector`
    ? LispVector<Reading<OPR, env, [[prev]]>>
  : U extends `map`
    ? LispMap<Reading<OPR, env, [[prev]]>>
  : U extends `filter`
    ? LispFilter<Reading<OPR, env, [[prev]]>>
  : U extends `remove`
    ? LispRemove<Reading<OPR, env, [[prev]]>>
  : U extends `reduce`
    ? LispReduce<Reading<OPR, env, [[prev]]>>
  : U extends `count`
    ? LispCount<Reading<OPR, env, [[prev]]>>
  : U extends `concat`
    ? LispConcat<Reading<OPR, env, [[prev]]>>
  : U extends `conj`
    ? LispConj<Reading<OPR, env, [[prev]]>>
  : U extends `first`
    ? LispFirst<Reading<OPR, env, [[prev]]>>
  : U extends `second`
    ? LispSecond<Reading<OPR, env, [[prev]]>>
  : U extends `last`
    ? LispLast<Reading<OPR, env, [[prev]]>>
  : U extends `rest`
    ? LispRest<Reading<OPR, env, [[prev]]>>
  : U extends `butlast`
    ? LispButlast<Reading<OPR, env, [[prev]]>>
  : U extends `reverse`
    ? LispReverse<Reading<OPR, env, [[prev]]>>
  : U extends `interleave`
    ? LispInterleave<Reading<OPR, env, [[prev]]>>
  : U extends `take`
    ? LispTake<Reading<OPR, env, [[prev]]>>
  : U extends `drop`
    ? LispDrop<Reading<OPR, env, [[prev]]>>
  : U extends `assoc-in`
    ? LispAssocIn<Reading<OPR, env, [[prev]]>>
  : U extends `update-in`
    ? LispUpdateIn<Reading<OPR, env, [[prev]]>>
  : U extends `assoc`
    ? LispAssoc<Reading<OPR, env, [[prev]]>>
  : U extends `update`
    ? LispUpdate<Reading<OPR, env, [[prev]]>>
  : U extends `get`
    ? LispGet<Reading<OPR, env, [[prev]]>>
  : U extends `get-in`
    ? LispGetIn<Reading<OPR, env, [[prev]]>>
  : U extends `eq` | `=`
    ? LispEq<Reading<OPR, env, [[prev]]>>
  : U extends `not`
    ? LispNot<Reading<OPR, env, [[prev]]>>
  : U extends `and`
    ? LispAnd<Reading<OPR, env, [[prev]]>>
  : U extends `or`
    ? LispOr<Reading<OPR, env, [[prev]]>>
  : U extends `inc`
    ? LispInc<Reading<OPR, env, [[prev]]>>
  : U extends `dec`
    ? LispDec<Reading<OPR, env, [[prev]]>>
  : U extends `+`
    ? LispAdd<Reading<OPR, env, [[prev]]>>
  : U extends `-`
    ? LispSub<Reading<OPR, env, [[prev]]>>
  : U extends `*`
    ? LispMul<Reading<OPR, env, [[prev]]>>
  : U extends `/`
    ? LispDiv<Reading<OPR, env, [[prev]]>>
  : U extends `mod` | `%`
    ? LispMod<Reading<OPR, env, [[prev]]>>
  : U extends `>` | `<` | `>=` | `<=`
    ? LispRelation<U, Reading<OPR, env, [[prev]]>>
  : U extends `number?`
    ? LispIsNumber<Reading<OPR, env, [[prev]]>>
  : U extends `string?`
    ? LispIsString<Reading<OPR, env, [[prev]]>>
  : U extends `vector?`
    ? LispIsVector<Reading<OPR, env, [[prev]]>>
  : U extends `map?`
    ? LispIsMap<Reading<OPR, env, [[prev]]>>
  : U extends `fn?`
    ? LispIsFn<Reading<OPR, env, [[prev]]>>
  : U extends `keyword?`
    ? LispIsKeyword<Reading<OPR, env, [[prev]]>>
  : U extends `ifn?`
    ? LispIsIfn<Reading<OPR, env, [[prev]]>>
  : U extends `pos-int?`
    ? LispIsPosInt<Reading<OPR, env, [[prev]]>>
  : U extends `neg-int?`
    ? LispIsNegInt<Reading<OPR, env, [[prev]]>>
  : U extends `odd?`
    ? LispIsOdd<Reading<OPR, env, [[prev]]>>
  : U extends `even?`
    ? LispIsEven<Reading<OPR, env, [[prev]]>>
  : U extends `zero?`
    ? LispIsZero<Reading<OPR, env, [[prev]]>>
  : U extends `symbol?`
    ? LispIsSymbol<Reading<OPR, env, [[prev]]>>
  : U extends `empty?`
    ? LispIsEmpty<Reading<OPR, env, [[prev]]>>
  : U extends `every?`
    ? LispIsEvery<Reading<OPR, env, [[prev]]>>
  : U extends `some`
    ? LispSome<Reading<OPR, env, [[prev]]>>
  : U extends `nil?`
    ? LispIsNil<Reading<OPR, env, [[prev]]>>
  : U extends `boolean?`
    ? LispIsBoolean<Reading<OPR, env, [[prev]]>>
  : U extends `type`
    ? LispType<Reading<OPR, env, [[prev]]>>
  : U extends `some?`
    ? LispIsSome<Reading<OPR, env, [[prev]]>>
  : Eval<[ReadLet<U, env>, OPR[0]], env, [prev]>

type EvalError1 = "EvalError1";
type EvalError2 = "EvalError2";
type EvalError3 = "EvalError3";
type EvalError4 = "EvalError4";
type EvalError5 = "EvalError5";
type EvalError6 = "EvalError6";
type EvalError7 =
"EvalError7"
type EvalError8 = "EvalError8";
type EvalError9 = "EvalError9";
type EvalError10 = "EvalError10";
type EvalError11 =
"EvalError11"
type EvalError12 = "EvalError12";
type EvalError13 = "EvalError13"
type EvalError14 = "EvalError14"
type EvalError15 = "EvalError15"
type EvalError16 = "EvalError16"
type EvalError17 = "EvalError17"
type EvalError18 = "EvalError18"
type EvalError19 = "EvalError19"
type EvalError20 = "EvalError20"

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
          ? Eval<[If<Eval<IFCond, env, [[prev]]>, IFT, IFF>, OPR[0]], env, [prev]>
        : OPC extends Sym & [`sym`, infer U]
          ? ReadLet<U, env> extends TNotMatch
            ? Builtins<U,OPR,env,prev>
          : ReadLet<U, env> extends Fn | Keyword | TMap & infer UU
            ? Eval<[UU, ...OPR], env, [prev]>
          : ReadLet<U, env> extends ['sym', BuiltinsUnion] & infer UU
            ? Eval<[UU, ...OPR], env, [prev]>
          : ErrorCase<EvalError3, `1st arg should be fn/keyword/map.`, A, env>
        : IsKeyMapSexpr<ReadLetRecur<A, env>, env> extends true
          ? IsKeyword<OPC> extends true
            ? LispGet<Reading<[...OPR, OPC], env, [[prev]]>>
          : LispGet<Reading<[OPC, ...OPR], env, [[prev]]>>
        : OPC extends LetForm
          ? Eval<[Eval<OPC, env, [[prev]]>, ...OPR], env, [prev]>
        : OPC extends Sexpr
          ? Eval<[Eval<OPC, env, [[prev]]>, ...OPR], env, [prev]>
        : ErrorCase<EvalError4, `the 1st is not a symbol but it should be.`, A, env>
      : ErrorCase<EvalError6, `env 1st should not be [].`, A, env>
    : ErrorCase<EvalError2, ``, A, env>
  : A extends IfForm & [`if`, infer IFCond, infer IFT, infer IFF]
    ? Eval<If<Eval<IFCond, env, [[prev]]>, IFT, IFF>, env, [prev]>
  : A extends Atom
    ? A extends Prim
      ? A
    : A extends Vector & ['vec', ...infer vr]
      ? vr extends []
        ? Vscope extends true
          ? []
        : ['vec']
      : vr extends [infer va, ...infer vb]
        ? [...(Vscope extends true ? [] : ['vec'])
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
      : ErrorCase<EvalError1, 'sym is not desconstructed well as an inner expression.', A, env>
    : ReadLetRecur<A, env> extends infer a
      ? a
    : never
  : A extends LetForm
    ? A extends [`let`, [Sym[], LetVal[]], Sexpr]
      ? A extends [`let`, [infer letsyms, infer letvals], infer LC]
        ? Eval<[`let`, Interleave<letsyms, letvals>, LC], env, [prev]>
      : ErrorCase<EvalError5, '', A, env>
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
        ? Eval<LC, Let<LN, LV, env>, [prev]>
      : LV extends Sexpr | Atom
        ? Eval<LV, env, [[prev]]> extends infer ValueEvaluated
          ? ValueEvaluated extends {error: string}
            ? { sexpr: ValueEvaluated
              , message: 'Invalid binding in let form.'
              , error: EvalError12 }
          : Eval<LC, Let<LN, ValueEvaluated, env>, [prev]>
        : never
      : LV extends IfForm
        ? Eval<LC, Eval<LV, env, [[prev]]>, [prev]>
      : ErrorCase<EvalError7, '', LV, env>
    : A extends ['let', [], infer Sexpr]
      ? Eval<Sexpr, env, [prev]>
    : ErrorCase<EvalError8, 'this is not proper let-form.', A, env>
  : { sexpr: A
    , message: `Some of Elem isn't Sexpr.`
    , error: EvalError11 } 

const evalatomtest: Eval<[`prim`, `'test'`]> = [`prim`, `'test'`];
const evalatomtest2: Eval<[`sym`, `test`], [[MakeVar<`test`, `'testval'`>]]> = [
  `prim`,
  `'testval'`,
];
const evalatomtest3: Eval<
  [`sym`, `test`],
  [[MakeVar<`test`, [`prim`, `'prim/test'`]>]]
> = [`prim`, `'prim/test'`];
const evalatomtest4: Eval<
  [`sym`, `test`],
  [[MakeVar<`test`, [`fn`, [[`sym`, `a`]], [`sym`, `a`]]>]]
> = [`fn`, [[`sym`, `a`]], [`sym`, `a`]];
const evalprimerrortest: Eval<[`prim`, 0]> = [`prim`, 0];

// ----------------------------
// -- Main
// ----------------------------

export namespace Cion {
  export type RawLisp<S extends string> = Eval<Compiler.SCompiler<Compiler.SParser<Compiler.SPad<S>>>>
  export type Lisp<S extends string> = Compiler.Unparse<RawLisp<S>>
  export type CionParser<S extends string> = Compiler.SParser<Compiler.SPad<S>>
  export type Builtins = BuiltinsUnion
}

export default Cion
