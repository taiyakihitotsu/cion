import type Bit from './bit.ts';
import type Compiler from './compiler';
type LetVal = string | Each | Each[];
type LetForm = [`let`, (Sym | LetVal)[] | [Sym[], LetVal[]], Each | Each[] | Sexpr];
type Each = LetForm | IfForm | Atom;
type Atom = ['map', Atom[]] | Sym | Prim | Fn | Vector | Keyword | Nil;
type TMap = Exclude<Atom, Sym | Prim | Fn | Vector | Keyword | Nil>;
type Sexpr = Array<Each | Each[] | Sexpr>;
type Nil = [];
declare const Nil: Nil;
type Keyword = [`key`, string];
type Sym = [`sym`, string];
type PrimString = ['prim', string];
type PrimBoolean = ['prim', boolean];
type PrimTestNumber = ['prim', number];
type PrimNumber = ['prim', string];
type Prim = PrimString | PrimBoolean | PrimNumber | PrimTestNumber;
type Args = Sym[];
type Fn = [`fn`, Args, Each | Each[] | Sexpr | Sexpr[]];
type Vector = [`vec`, ...(Sexpr | LetForm | Atom)[]] | [`vec`];
type Var = {
    name: string;
    value: string | Atom;
};
type Env = [] | Var[];
type NotMatch = "NotMatch";
declare const NotMatch = "NotMatch";
type MakeVar<N, V> = {
    name: N;
    value: V;
};
type GetVar<T, E> = E extends Env ? E extends [infer U, ...infer R] ? U extends Var ? U["name"] extends T ? U["value"] : GetVar<T, R> : NotMatch : NotMatch : NotMatch;
type EnvLifo = Env[];
type LetError0 = "LetError";
type Let<N, V, EnvLifo = Env[]> = EnvLifo extends Env[] ? [...EnvLifo, [MakeVar<N, V>]] : LetError0;
type ReadLet<N, EnvLifo = [[]]> = EnvLifo extends [...infer HS, infer L] ? L extends Env ? GetVar<N, L> extends NotMatch ? ReadLet<N, HS> : GetVar<N, L> : NotMatch : NotMatch;
type ReadLetRecur<Sexpr, env, R extends unknown[] = []> = Sexpr extends [infer F, ...infer rest] ? F extends Sym ? ReadAtom<F, env> extends infer P ? ReadLetRecur<rest, env, [...R, P extends NotMatch ? F : P]> : never : ReadLetRecur<rest, env, [...R, F extends unknown[] ? ReadLetRecur<F, env, []> : F]> : R;
type ReadAtom<A, EnvLifo = [[]], prev = 0> = A extends [`sym`, infer S] ? ReadLet<S, EnvLifo> : Eval<A, EnvLifo, [prev]>;
type ReadingError0 = "ReadingError0";
type ReadingError1 = "ReadingError1";
type Reading<AS, EnvLifo = [[]], prev = 0, R = [], IsFn extends boolean = false> = R extends Atom[] ? AS extends [infer H, ...infer T] ? H extends Atom ? Reading<T, EnvLifo, prev, [...R, ReadAtom<H, EnvLifo, prev>]> : H extends Sexpr | LetForm ? Reading<T, EnvLifo, prev, [...R, Eval<H, EnvLifo, prev>]> : {
    sexpr: AS;
    error: ReadingError1;
} : R : {
    sexpr: R;
    error: ReadingError0;
    message: `sexpr is not atom list.`;
};
type AppendError = "AppendError";
type AppendP<S> = S extends [`prim`, `'${infer U}'`] ? [`prim`, `'+${U}'`] : {
    error: [AppendError, S];
};
type StrError0 = "StrError0";
type Str<S, R = ""> = R extends string ? S extends [[`prim`, `${infer HS}`], ...infer T] ? HS extends `'${infer hs}'` | `'${infer hs}'` ? Str<T, `${R}${hs}`> : Str<T, `${R}${HS}`> : [`prim`, `'${R}'`] : StrError0;
type _And<Fst, Snd> = Fst extends false ? false : Snd extends false ? false : true;
type _LispAnd<S> = S extends [infer Fst, ...infer Rest] ? Fst extends [`prim`, infer Boolean] ? Boolean extends `nil` ? false : Rest extends [] ? Boolean : _And<Boolean, _LispAnd<Rest>> : never : never;
type LispAnd<S> = S extends [infer _, ...infer __] ? [`prim`, _LispAnd<S>] : [`prim`, false];
type _LispOr<S> = S extends [] ? false : S extends [infer Fst, ...infer Rest] ? Fst extends [`prim`, false] | [`prim`, 'nil'] ? _LispOr<Rest> : true : never;
type LispOr<S> = S extends [infer _, ...infer __] ? [`prim`, _LispOr<S>] : [`prim`, false];
type _Eq<Fst, Snd> = Fst extends Snd ? (Snd extends Fst ? Fst : never) : never;
type _LispEq<S> = S extends [infer Fst, ...infer Rest] ? Rest extends [] ? Fst : _Eq<Fst, _LispEq<Rest>> : never;
type LispEqError0 = "LispEqError0";
type LispEq<S> = S extends [infer Fst, ...infer Rest] ? [`prim`, Eq<Fst, _LispEq<S>>] : {
    error: LispEqError0;
    sexpr: S;
};
type _Not<B> = B extends false ? true : false;
type LispNot<S> = S extends [['prim', infer U]] ? ['prim', _Not<U>] : never;
type LispAddError0 = 'LispAddError0';
type LispAddError1 = 'LispAddError1';
type LispAdd<S, R extends string = "00000000"> = S extends [] ? [`prim`, R] : S extends [infer Fst, ...infer Rest] ? Fst extends [`prim`, infer FstP extends string] ? LispAdd<Rest, Bit.BitAdd<R, FstP>> : {
    error: LispAddError0;
    sexpr: S;
} : {
    error: LispAddError1;
    sexpr: S;
};
type LispSubError0 = 'LispSubError0';
type LispSub<S, R extends string = "00000000", Init extends boolean = true> = S extends [] ? [`prim`, R] : S extends [[`prim`, infer Fst extends string], ...infer Rest extends string[][]] ? Init extends true ? LispSub<Rest, Fst, false> : LispSub<Rest, Bit.BitSub<R, Fst>, false> : {
    error: [LispSubError0];
};
type LispMulError0 = 'LispMulError0';
type LispMul<S, R extends string = "00000000", Init extends boolean = true> = S extends [] ? [`prim`, R] : S extends [[`prim`, infer Fst extends string], ...infer Rest extends string[][]] ? Init extends true ? LispMul<Rest, Fst, false> : LispMul<Rest, Bit.BitMul<R, Fst>, false> : {
    error: [LispMulError0];
};
type LispDivError0 = 'LispDivError0';
type LispDivError1 = 'LispDivError1';
type LispDiv<S, R extends string = "00000001", Init extends boolean = true> = S extends [] ? [`prim`, R] : S extends [[`prim`, infer Fst extends string], ...infer Rest extends string[][]] ? Init extends true ? LispDiv<Rest, Fst, false> : Bit.BitDiv<R, Fst> extends Bit.Nil | string & infer Div ? Div extends string ? LispDiv<Rest, Div, false> : Bit.Nil : {
    error: [LispDivError0];
} : {
    error: [LispDivError1];
};
type LispModError0 = 'LispModError0';
type LispModError1 = 'LispModError1';
type LispMod<S, R extends string = "0000000000000000", Init extends boolean = true> = S extends [] ? [`prim`, R] : S extends [[`prim`, infer Fst extends string], ...infer Rest extends string[][]] ? Init extends true ? Bit.BitGTE<Fst, '0000000000000000'> extends true ? LispMod<Rest, Fst, false> : Rest extends [['prim', infer Snd extends string]] ? LispMod<[['prim', Bit.BitSub<Snd, Bit.BitRevSign<Fst>>], ['prim', Snd]]> : never : Bit.BitMod<R, Fst> extends Bit.Nil | string & infer Mod ? Mod extends string ? LispMod<Rest, Mod, false> : Bit.Nil : {
    error: [LispModError0];
} : {
    error: [LispModError1];
};
type LispRelationError0 = 'LispRelationError0';
type LispRelationError1 = 'LispRelationError1';
type LispRelation<Name extends string, S, R extends string = "00000000", Init extends boolean = true, Next extends boolean = true> = S extends [] ? [`prim`, Next] : S extends [[`prim`, infer Fst extends string], ...infer Rest extends string[][]] ? Init extends true ? LispRelation<Name, Rest, Fst, false, Next> : Next extends true ? Name extends '>' ? LispRelation<Name, Rest, Fst, false, Bit.BitGT<R, Fst>> : Name extends '<' ? LispRelation<Name, Rest, Fst, false, Bit.BitLT<R, Fst>> : Name extends '>=' ? LispRelation<Name, Rest, Fst, false, Bit.BitGTE<R, Fst>> : Name extends '<=' ? LispRelation<Name, Rest, Fst, false, Bit.BitLTE<R, Fst>> : {
    error: [LispRelationError0];
} : [`prim`, false] : {
    error: [LispRelationError1];
};
type EvalError2 = "EvalError2";
type EvalError3 = "EvalError3";
type EvalError4 = "EvalError4";
type EvalError6 = "EvalError6";
type EvalError7 = "EvalError7/ 2nd in let form must be Atom. No wrapped value is deprecated.";
type EvalError8 = "EvalError8";
type EvalError11 = "EvalError11/ Some of elements type in SEXPR doesn't satisfy EACH.";
type Eq<L, R> = L extends R ? (R extends L ? true : false) : false;
type If<A, B, C> = A extends [`prim`, true] ? B : C;
type IfForm = [`if`, Each | Sexpr, Each | Sexpr, Each | Sexpr];
type NatNumber = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
type OddNumber = '1' | '3' | '5' | '7' | '9';
type IsNumber<S extends string> = S extends `${infer F}${infer R}` ? F extends NatNumber ? R extends '' ? true : IsNumber<R> : false : false;
type _IsOdd<S extends string> = S extends `${infer F}${infer R}` ? R extends '' ? F extends OddNumber ? true : false : _IsOdd<R> : false;
type IsOdd<S extends string> = true extends IsNumber<S> ? _IsOdd<S> : false;
type IsEven<S extends string> = true extends IsNumber<S> ? true extends _IsOdd<S> ? false : true : false;
type LispIsNumber<S> = S extends [['prim', infer N extends string]] ? N extends `1${infer rN}` ? ['prim', IsNumber<rN>] : ['prim', IsNumber<N>] : ['prim', false];
type LispIsString<S> = S extends [['prim', infer N extends string]] ? N extends `'${infer _}'` ? ['prim', true] : ['prim', false] : ['prim', false];
type LispIsVector<S> = S extends [['vec', ...infer _]] ? ['prim', true] : ['prim', false];
type LispIsMap<S> = S extends [['map', ...infer _]] ? ['prim', true] : ['prim', false];
type LispIsFn<S> = S extends [['fn', ...infer _]] ? ['prim', true] : ['prim', false];
type LispIsKeyword<S> = S extends [['key', infer _]] ? ['prim', true] : ['prim', false];
type LispIsIfn<S> = LispIsKeyword<S> extends ['prim', false] ? LispIsFn<S> : ['prim', true];
type LispIsPosInt<S> = S extends [['prim', infer N extends string]] ? N extends `1${infer _}` ? ['prim', false] : LispIsNumber<S> : ['prim', false];
type LispIsNegInt<S> = S extends [['prim', infer N extends string]] ? N extends `1${infer _}` ? LispIsNumber<S> : ['prim', false] : ['prim', false];
type LispIsOdd<S> = S extends [['prim', infer N extends string]] ? ['prim', IsOdd<N>] : ['prim', false];
type LispIsEven<S> = S extends [['prim', infer N extends string]] ? ['prim', IsEven<N>] : ['prim', false];
type LispIsZero<S> = S extends [['prim', infer N extends string]] ? N extends '0' ? ['prim', true] : ['prim', false] : ['prim', false];
type LispIsSymbol<S> = S extends [['sym', infer _]] ? ['prim', true] : ['prim', false];
type LispIsEmpty<S> = S extends [['vec', ...infer V]] ? V extends [] ? ['prim', true] : ['prim', false] : ['prim', false];
type ConcatError0 = "ConcatError0";
type TConcat<V extends Array<Array<unknown>>, Stack extends Array<unknown> = []> = V['length'] extends 0 ? Stack : V extends [infer Head extends Array<unknown>, ...infer Rest extends Array<Array<unknown>>] ? TConcat<Rest, [...Stack, ...Head]> : never;
type LispConcat<S, R extends unknown[][] = []> = S extends Vector[] & [['vec', ...infer H], ...infer T] ? T extends [] ? ['vec', ...TConcat<[...R, H]>] : LispConcat<T, [...R, H]> : ConcatError0;
type IsKeyword<T> = T extends ['key', `:${infer S}`] ? true : false;
type IsMap<T> = T extends TMap ? true : false;
type IsKeyMapSexpr<S, env = [[]]> = S extends [infer Fst, infer Snd] ? IsKeyword<Eval<Fst, env>> extends true ? IsMap<Eval<Snd, env>> extends true ? true : false : IsKeyword<Eval<Snd, env>> extends true ? IsMap<Eval<Fst, env>> extends true ? true : false : false : false;
type GetMap<K, V, sV = V extends [infer _, infer i] ? i : never> = sV extends [infer k, infer v, ...infer _] ? k extends K ? v : GetMap<K, V, sV extends [infer _, infer __, ...infer i] ? i : never> : Nil;
type GetVecError0 = 'GetVecError0';
type GetVecError1 = 'GetVecError1';
type GetVec<Idx extends PrimNumber, Vec extends Vector> = Vec extends ['vec', infer H, ...infer T extends Atom[]] ? Idx extends PrimNumber & ['prim', infer idx extends string] ? Bit.BitIsZero<idx> extends true ? H : Bit.BitGT<idx, "0"> extends true ? T extends [] ? Nil : GetVec<['prim', Bit.BitSub<idx, "1">], ['vec', ...T]> : GetVecError0 : GetVecError1 : Nil;
type GetError0 = 'GetError0';
type Get<K extends PrimNumber | Keyword, V extends Vector | TMap> = V extends Vector ? K extends PrimNumber ? GetVec<K, V> : GetError0 : GetMap<K, V>;
type LispGetError0 = "LispGetError0";
type LispGet<S> = S extends [infer Map extends TMap, infer Key extends Keyword] ? Get<Key, Map> : S extends [infer Vec extends Vector, infer Idx extends ['prim', string]] ? Get<Idx, Vec> : {
    error: [LispGetError0, S, "this is not map and key or vector and idx-num."];
    sexpr: S;
};
type AssocError0 = 'Args: 1st Map & 2nd not keyword.';
type AssocError1 = 'Args: 1st Vector & 2nd not number.';
declare const AssocError0 = "Args: 1st Map & 2nd not keyword.";
declare const AssocError1 = "Args: 1st Vector & 2nd not number.";
type _Assoc<M, K extends Keyword | PrimNumber, V extends Atom, Type extends 'assoc' | 'update' = 'assoc', S extends unknown[] = []> = M extends Vector & ['vec', infer mV extends Atom, ...infer mR extends Atom[]] ? K extends PrimNumber & ['prim', infer kB extends string] ? Bit.BitIsZero<kB> extends true ? ['vec', ...S, Type extends 'update' ? Eval<[V, mV]> : V, ...mR] : Bit.BitGT<kB, '0'> extends true ? mR extends [] ? ['vec', ...S, mV] : _Assoc<['vec', ...mR], ['prim', Bit.BitSub<kB, '1'>], V, Type, [...S, mV]> : ['vec', ...S, V, ...mR] : AssocError1 : M extends TMap & ['map', [infer mK extends Keyword, infer mV extends Atom, ...infer mR]] ? mK extends K ? ['map', [...S, mK, Type extends 'update' ? Eval<[V, mV]> : V, ...mR]] : mR extends [] ? ['map', [...S, mK, mV, K, V]] : _Assoc<['map', mR], K, V, Type, [...S, mK, mV]> : AssocError0;
type AssocInError0 = 'AssocInError0';
type AssocInError3 = 'AssocInError3';
type AssocInError4 = 'AssocInError4';
type AssocInError5 = 'AssocInError5';
type AssocInError6 = 'AssocInError6';
type AssocInError7 = 'AssocInError7';
type AssocInError8 = 'AssocInError8';
type _rAssocIn<M extends Vector | TMap, Kh extends (Keyword | PrimNumber), Kt extends (Keyword | PrimNumber)[], V extends Atom, Type extends 'update' | 'assoc' = 'assoc'> = Kt extends [] ? _Assoc<M, Kh, V, Type> : Get<Kh, M> extends infer Next ? Next extends Vector | TMap ? _AssocIn<Next, ['vec', ...Kt], V, Type> extends infer Recur ? Recur extends Atom ? _Assoc<M, Kh, Recur> : {
    error: AssocInError7;
    message: `The value of key (${Kt[0][1]}) is not vector nor map.`;
} : AssocInError3 : {
    error: AssocInError8;
    message: "Keys rests but its value is not vector nor map.";
} : AssocInError4;
type _AssocIn<M extends Vector | TMap, Ks extends ['vec', ...unknown[]], V extends Atom, Type extends 'update' | 'assoc' = 'assoc'> = M extends Vector ? M extends ['vec'] ? M : Ks extends ['vec', infer Kh extends PrimNumber, ...infer Kt extends (Keyword | PrimNumber)[]] ? _rAssocIn<M, Kh, Kt, V, Type> : AssocInError0 : M extends TMap ? Ks extends ['vec', infer Kh extends Keyword, ...infer Kt extends (Keyword | PrimNumber)[]] ? _rAssocIn<M, Kh, Kt, V, Type> : AssocInError5 : AssocInError6;
type _Update<M, K extends Keyword | PrimNumber, F extends Fn> = _Assoc<M, K, F, 'update'>;
type _UpdateIn<M extends Vector | TMap, K extends ['vec', ...unknown[]], F extends Fn> = _AssocIn<M, K, F, 'update'>;
type LispAssocError0 = 'LispAssocError0';
type LispAssoc<S> = S extends [
    infer M extends Vector | TMap,
    infer K extends Keyword | PrimNumber,
    infer V extends Atom
] ? _Assoc<M, K, V> : LispAssocError0;
type LispAssocInError0 = 'LispAssocInError0';
type LispAssocIn<S> = S extends [
    infer M extends Vector | TMap,
    infer Ks extends ['vec', ...(Keyword | PrimNumber)[]],
    infer V extends Atom
] ? _AssocIn<M, Ks, V> : LispAssocInError0;
type LispUpdateError0 = 'LispUpdateError0';
type LispUpdate<S> = S extends [
    infer M extends Vector | TMap,
    infer K extends Keyword | PrimNumber,
    infer V extends Fn
] ? _Update<M, K, V> : LispUpdateError0;
type LispUpdateInError0 = 'LispUpdateInError0';
type LispUpdateIn<S> = S extends [
    infer M extends Vector | TMap,
    infer Ks extends ['vec', ...(Keyword | PrimNumber)[]],
    infer V extends Fn
] ? _UpdateIn<M, Ks, V> : LispUpdateInError0;
type LispVector<S> = S extends unknown[] ? ['vec', ...S] : {
    error: [LispGetError0, S, ""];
    sexpr: S;
};
type FirstError0 = "FirstError0";
type FirstError1 = "FirstError1";
type RestError0 = "RestError0";
type RestError1 = "RestError1";
type First<V> = V extends Vector & [`vec`, infer H, ...infer T] ? H : V extends ['vec'] ? Nil : FirstError0;
type LispFirst<S> = S extends [infer V extends Vector] ? First<V> : {
    error: FirstError1;
    message: S;
};
type LastError0 = 'LastError0';
type LastError1 = 'LastError1';
type Last<V> = V extends [infer H, ...infer T] ? T extends [] ? H : Last<T> : LastError0;
type LispLast<S> = S extends [['vec']] ? Nil : S extends [['vec', ...infer V]] ? Last<V> : LastError1;
type Rest<V> = V extends Vector & [`vec`, infer H, ...infer T] ? T[0] extends Atom ? [`vec`, ...T] : [`vec`] : RestError0;
type LispRest<S> = S extends [infer V extends Vector] ? Rest<V> : RestError1;
type ButlastError0 = "ButlastError0";
type ButlastError1 = "ButlastError1";
type ButlastError2 = "ButlastError2";
type ButlastError3 = "ButlastError3";
type _Butlast<V, R extends unknown[] = []> = V extends [infer H, ...infer T] ? T extends [] ? R : _Butlast<T, [...R, H]> : ButlastError0;
type Butlast<V> = V extends Vector & ['vec', ...infer v] ? _Butlast<v> extends Atom[] & infer a ? ['vec', ..._Butlast<v>] : ButlastError1 : ButlastError3;
type LispButlast<S> = S extends [infer V extends Vector] ? Butlast<V> : ButlastError2;
type ConjError2 = "ConjError2";
type LispConj<S> = S extends [infer H extends Vector, ...infer T extends Atom[]] ? [...H, ...T] : ConjError2;
type TakeError0 = "TakeError0";
type TakeError1 = "TakeError1";
type TakeError2 = "TakeError2";
type TakeError3 = "TakeError3";
type Take<N extends string, V extends unknown[], R extends unknown[] = []> = V extends [] ? R : Bit.BitGTE<"0", N> extends true ? R : V extends [infer F, ...infer T] ? Take<Bit.BitSub<N, "1">, T, [...R, F]> : TakeError0;
type LispTake<S> = S extends [['prim', infer N extends string], ['vec', ...infer V]] ? Take<N, V> extends infer RV ? RV extends unknown[] ? ['vec', ...RV] : TakeError2 : TakeError3 : TakeError1;
type DropError0 = "DropError0";
type DropError1 = "DropError1";
type DropError2 = "DropError2";
type DropError3 = "DropError3";
type Drop<N extends string, V extends unknown[], R extends unknown[] = []> = V extends [] ? R : V extends [infer _, ...infer T] ? Bit.BitGTE<"0", N> extends true ? V : Drop<Bit.BitSub<N, "1">, T> : DropError0;
type LispDrop<S> = S extends [['prim', infer N extends string], ['vec', ...infer V]] ? Drop<N, V> extends infer RV ? RV extends unknown[] ? ['vec', ...RV] : DropError2 : DropError3 : DropError1;
type FMapError = "MapError";
type FilterError = "FilterError";
type _FMap<F, V, Env = [[]], prev = [0]> = V extends Vector ? V extends [`vec`, infer H, ...infer T] ? T[0] extends Atom ? [Eval<[F, H]>, ..._FMap<F, [`vec`, ...T]>] : [Eval<[F, H]>] : [] : [];
type FMap<F, V, Env = [[]], prev = [0]> = [`vec`, ..._FMap<F, V>];
type LispMap<S> = S extends [infer f, infer vs] ? FMap<f, vs> : FMapError;
type FilterError0 = "FilterError0";
type FilterError1 = "FilterError1";
type _Filter<F, V, Env = [[]], prev = [0]> = V extends Vector ? V extends [`vec`, infer H, ...infer T] ? T extends [] ? Eval<[F, H]> extends [`prim`, true] ? [H] : [] : Eval<[F, H]> extends [`prim`, true] ? [H, ..._Filter<F, [`vec`, ...T]>] : [..._Filter<F, [`vec`, ...T]>] : [{
    error: FilterError0;
}] : [{
    error: FilterError1;
}];
type Filter<F, V, Env = [[]], prev = [0]> = [`vec`, ..._Filter<F, V>];
type LispFilter<S> = S extends [infer f, infer vs] ? Filter<f, vs> : FilterError;
type LispRemove<S> = S extends [infer f, infer vs] ? Filter<['fn', [['sym', 'aaa']], [['sym', 'not'], [f, ['sym', 'aaa']]]], vs> : FilterError;
type InterleaveError1 = "InterleaveError1";
type Interleave<V, W> = V extends [infer HeadV, ...infer TailV] ? W extends [infer HeadW, ...infer TailW] ? TailW extends never ? [] : TailV extends never ? [] : [HeadV, HeadW, ...Interleave<TailV, TailW>] : [] : [];
type LispInterleave<S> = S extends [['vec', ...infer V], ['vec', ...infer W]] ? ['vec', ...Interleave<V, W>] : InterleaveError1;
type ReduceError0 = 'ReduceError0';
type ReduceError1 = 'ReduceError1';
type ReduceError2 = 'ReduceError2';
type _Reduce<F, Init, V> = V extends [infer H, ...infer T] ? T['length'] extends 0 ? Eval<[F, Init, H]> : _Reduce<F, Eval<[F, Init, H]>, T> : {
    error: [ReduceError0];
};
type Reduce<F, Init, V> = V extends ['vec', ...infer v] ? _Reduce<F, Init, v> : {
    error: [ReduceError1];
};
type LispReduce<S> = S extends [infer f, infer init, infer v] ? Reduce<f, init, v> : ReduceError2;
type ReverseError0 = 'ReverseError0';
type ReverseError1 = 'ReverseError1';
type ReverseError2 = 'ReverseError2';
type ReverseError3 = 'ReverseError3';
type Reverse<V, R extends Array<unknown> = []> = V extends [infer H, ...infer T] ? T['length'] extends 0 ? [H, ...R] : Reverse<T, [H, ...R]> : V extends [] ? [] : {
    error: [ReverseError0];
};
type LispReverse<S> = S extends [Vector] & [['vec', ...infer V]] ? Reverse<V> extends infer RV ? RV extends unknown[] ? ['vec', ...RV] : ReverseError1 : ReverseError2 : ReverseError3;
type InsertSecondError0 = 'InsertSecondError0';
type InsertSecond<V, E> = V extends [infer H, ...infer R] ? [H, E, ...R] : V extends [...infer R] ? [E, ...R] : {
    error: [InsertSecondError0];
};
type InsertLastError0 = 'InsertLastError0';
type InsertLast<V, E> = V extends [...infer R] ? [...R, E] : {
    error: [InsertLastError0];
};
type VecWrap<V> = V extends unknown[][] ? V : [V];
type ThreadFirstError1 = 'ThreadFirstError1';
type ThreadFirst<Fst, V extends unknown[], R extends unknown[] = [], Init extends boolean = true> = V['length'] extends 0 ? InsertSecond<VecWrap<Fst>, R> : V extends [infer Head, ...infer Tail] ? Init extends false ? ThreadFirst<Head, Tail, InsertSecond<VecWrap<Fst>, R>, false> : Tail extends [infer N, ...infer M] ? ThreadFirst<N, M, InsertSecond<VecWrap<Head>, Fst>, false> : InsertSecond<VecWrap<Head>, Fst> : {
    error: [ThreadFirstError1];
};
type LispThreadFirstError0 = 'LispThreadFirstError0';
type LispThreadFirst<V> = V extends [infer H, ...infer T] ? T['length'] extends 0 ? V : ThreadFirst<H, T> : {
    error: [LispThreadFirstError0];
};
type ThreadLastError1 = 'ThreadLastError1';
type ThreadLast<Fst, V extends unknown[], R extends unknown[] = [], Init extends boolean = true> = V['length'] extends 0 ? InsertLast<VecWrap<Fst>, R> : V extends [infer Head, ...infer Tail] ? Init extends false ? ThreadLast<Head, Tail, InsertLast<VecWrap<Fst>, R>, false> : Tail extends [infer N, ...infer M] ? ThreadLast<N, M, InsertLast<VecWrap<Head>, Fst>, false> : InsertLast<VecWrap<Head>, Fst> : {
    error: [ThreadLastError1];
};
type LispThreadLastError0 = 'LispThreadLastError0';
type LispThreadLast<V> = V extends [infer H, ...infer T] ? T['length'] extends 0 ? V : ThreadLast<H, T> : {
    error: [LispThreadLastError0];
};
type Eval<A, env = [[]], prev = 0, Vscope extends boolean = false> = A extends Sexpr ? A extends [infer OPC, ...infer OPR] ? env extends EnvLifo ? OPC extends Fn & [`fn`, infer syms, infer D] ? Eval<[`let`, Interleave<syms, OPR>, D], env, [prev]> : OPC extends IfForm & [`if`, infer IFCond, infer IFT, infer IFF] ? Eval<// point (A)
[
    If<Eval<IFCond, env, [[prev]]>, IFT, IFF>,
    OPR[0]
], env, [
    prev
]> : OPC extends Sym & [`sym`, infer U] ? ReadLet<U, env> extends NotMatch ? U extends `AppendP` ? AppendP<ReadAtom<Eval<OPR[0], env, [[prev]]>, env, [prev]>> : U extends '->' ? Eval<LispThreadFirst<OPR>, env, [[prev]]> : U extends '->>' ? Eval<LispThreadLast<OPR>, env, [[prev]]> : U extends `str` ? Str<Reading<OPR, env, [[prev]]>> : U extends `vector` ? LispVector<Reading<OPR, env, [[prev]]>> : U extends `map` ? LispMap<Reading<OPR, env, [[prev]]>> : U extends `filter` ? LispFilter<Reading<OPR, env, [[prev]]>> : U extends `remove` ? LispRemove<Reading<OPR, env, [[prev]]>> : U extends `reduce` ? LispReduce<Reading<OPR, env, [[prev]]>> : U extends `concat` ? LispConcat<Reading<OPR, env, [[prev]]>> : U extends `conj` ? LispConj<Reading<OPR, env, [[prev]]>> : U extends `first` ? LispFirst<Reading<OPR, env, [[prev]]>> : U extends `last` ? LispLast<Reading<OPR, env, [[prev]]>> : U extends `rest` ? LispRest<Reading<OPR, env, [[prev]]>> : U extends `butlast` ? LispButlast<Reading<OPR, env, [[prev]]>> : U extends `reverse` ? LispReverse<Reading<OPR, env, [[prev]]>> : U extends `interleave` ? LispInterleave<Reading<OPR, env, [[prev]]>> : U extends `take` ? LispTake<Reading<OPR, env, [[prev]]>> : U extends `drop` ? LispDrop<Reading<OPR, env, [[prev]]>> : U extends `assoc-in` ? LispAssocIn<Reading<OPR, env, [[prev]]>> : U extends `update-in` ? LispUpdateIn<Reading<OPR, env, [[prev]]>> : U extends `assoc` ? LispAssoc<Reading<OPR, env, [[prev]]>> : U extends `update` ? LispUpdate<Reading<OPR, env, [[prev]]>> : U extends `get` ? LispGet<Reading<OPR, env, [[prev]]>> : U extends `eq` | `=` ? LispEq<Reading<OPR, env, [[prev]]>> : U extends `not` ? LispNot<Reading<OPR, env, [[prev]]>> : U extends `and` ? LispAnd<Reading<OPR, env, [[prev]]>> : U extends `or` ? LispOr<Reading<OPR, env, [[prev]]>> : U extends `+` ? LispAdd<Reading<OPR, env, [[prev]]>> : U extends `-` ? LispSub<Reading<OPR, env, [[prev]]>> : U extends `*` ? LispMul<Reading<OPR, env, [[prev]]>> : U extends `/` ? LispDiv<Reading<OPR, env, [[prev]]>> : U extends `mod` | `%` ? LispMod<Reading<OPR, env, [[prev]]>> : U extends `>` | `<` | `>=` | `<=` ? LispRelation<U, Reading<OPR, env, [[prev]]>> : U extends `number?` ? LispIsNumber<Reading<OPR, env, [[prev]]>> : U extends `string?` ? LispIsString<Reading<OPR, env, [[prev]]>> : U extends `vector?` ? LispIsVector<Reading<OPR, env, [[prev]]>> : U extends `map?` ? LispIsMap<Reading<OPR, env, [[prev]]>> : U extends `fn?` ? LispIsFn<Reading<OPR, env, [[prev]]>> : U extends `keyword?` ? LispIsKeyword<Reading<OPR, env, [[prev]]>> : U extends `ifn?` ? LispIsIfn<Reading<OPR, env, [[prev]]>> : U extends `pos-int?` ? LispIsPosInt<Reading<OPR, env, [[prev]]>> : U extends `neg-int?` ? LispIsNegInt<Reading<OPR, env, [[prev]]>> : U extends `odd?` ? LispIsOdd<Reading<OPR, env, [[prev]]>> : U extends `even?` ? LispIsEven<Reading<OPR, env, [[prev]]>> : U extends `zero?` ? LispIsZero<Reading<OPR, env, [[prev]]>> : U extends `symbol?` ? LispIsSymbol<Reading<OPR, env, [[prev]]>> : U extends `empty?` ? LispIsEmpty<Reading<OPR, env, [[prev]]>> : Eval<[ReadLet<U, env>, OPR[0]], env, [prev]> : ReadLet<U, env> extends Fn | Keyword | TMap & infer UU ? Eval<[UU, ...OPR], env, [prev]> : {
    sexpr: A;
    error: EvalError3;
    msg: '1st arg should be fn/keyword/map.';
    env: env;
} : IsKeyMapSexpr<ReadLetRecur<A, env>, env> extends true ? IsKeyword<OPC> extends true ? LispGet<Reading<[...OPR, OPC], env, [[prev]]>> : LispGet<Reading<[OPC, ...OPR], env, [[prev]]>> : OPC extends LetForm ? Eval<[Eval<OPC, env, [[prev]]>, ...OPR], env, [prev]> : {
    error: EvalError4;
    message: 'the 1st is not a symbol but it should be.';
    sexpr: A;
} : {
    error: [EvalError6, "env 1st shouldn't be [].", prev, A];
    env: env;
    sexpr: A;
} : EvalError2 : A extends IfForm & [`if`, infer IFCond, infer IFT, infer IFF] ? Eval<If<Eval<IFCond, env, [[prev]]>, IFT, IFF>, env, [prev]> : A extends Atom ? A extends Prim ? A : A extends Vector & ['vec', ...infer vr] ? vr extends [] ? Vscope extends true ? [] : ['vec'] : vr extends [infer va, ...infer vb] ? [
    ...(Vscope extends true ? [] : ['vec']),
    (Eval<va, env, prev, va extends Vector ? false : true>),
    ...(Eval<['vec', ...vb], env, prev, true> extends infer u ? u extends unknown[] ? u : [] : [])
] : [] : A extends Sym & [`sym`, infer SS] ? ReadLet<SS, env> extends infer U ? U extends Atom ? U : U extends NotMatch ? A : [`prim`, U] : never : ReadLetRecur<A, env> extends infer a ? a : never : A extends LetForm ? A extends [`let`, [Sym[], LetVal[]], Sexpr] ? A extends [`let`, [infer letsyms, infer letvals], infer LC] ? Eval<[`let`, Interleave<letsyms, letvals>, LC], env, [prev]> : never : A extends [
    `let`,
    [
        [`sym`, infer LN],
        infer LV,
        ...infer LRest
    ],
    infer LC
] ? LRest extends [[`sym`, infer LRLN], infer LRLV, ...infer RRest] ? Eval<[
    `let`,
    [
        [`sym`, LN],
        LV
    ],
    [
        `let`,
        [[`sym`, LRLN], LRLV, ...RRest],
        LC
    ]
], env, [
    prev
]> : LV extends Prim & [`prim`, infer LP] ? Eval<LC, Let<LN, LV, env>, [prev]> : LV extends Sym & [`sym`, infer LP] ? Eval<LC, Let<LN, ReadLet<LP, env>, env>, [prev]> : LV extends LetForm ? Eval<[
    `let`,
    [[`sym`, LN], Eval<LV, env, [prev]>],
    LC
], env, [
    prev
]> : LV extends Fn ? Eval<LC, Let<LN, LV, env>, [prev]> : LV extends Sexpr | Atom ? Eval<LC, Let<LN, Eval<LV, env, [[prev]]>, env>, [prev]> : {
    error: [EvalError7, prev, A];
} : A extends ['let', [], infer Sexpr] ? Eval<Sexpr, env, [prev]> : {
    error: [EvalError8, "this is not proper let-form."];
    prev: prev;
    sexpr: A;
} : {
    error: [EvalError11, prev, A];
};
export declare namespace Cion {
    type RawLisp<S extends string> = Eval<Compiler.SCompiler<Compiler.SParser<Compiler.SPad<S>>>>;
    type Lisp<S extends string> = Compiler.Unparse<RawLisp<S>>;
}
export default Cion;
