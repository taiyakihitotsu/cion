import type Bit from './bit.ts'
import type Compiler from './compiler'


// type TMapAtom = ['map', TMapAtom[]] | Atom
// type TMap = Exclude<TMapAtom, Atom>

// type Each = LetForm | IfForm | Atom
// type Atom = Sym | Prim | Fn | Vector | Keyword | Nil
// todo : naming
// let itself isn't value and returning value, unlike fn, so maybe ok as it is.
// todo
// integrated let form to [[`sym`, string], EACH | Array<EACH>] in future.
type LetVal = string | Each | Each[];
type LetArg = [Sym, LetVal];
// todo : too ugly.
type LetForm = [`let`, (Sym | LetVal)[] | [Sym[], LetVal[]], Each | Each[] | Sexpr];
// test let
const larttest: LetArg = [[`sym`, `t`], `test`];

type Each = LetForm | IfForm | Atom
type Atom = ['map', Atom[]] | Sym | Prim | Fn | Vector | Keyword | Nil
type TMap = Exclude<Atom, Sym | Prim | Fn | Vector | Keyword | Nil>

// This isn't usually way to define a sexpr, not including atomic something.
// That role leaves to EACH.
type Sexpr = Array<Each | Each[] | Sexpr>;
type Nil = [];
const Nil: Nil = [];
// type Nil = [`prim`, 'nil']
// const Nil = [`prim`, 'nil']

type Keyword = [`key`, string]
type Sym = [`sym`, string];
type Prim = [`prim`, string | boolean | number]; // todo : This boolean is appended IFForm, using boolean directly in current.
type Args = Sym[];
// type Fn = [`fn`, Args, Each | Array<Each>];
// type Fn = [`fn`, Args, Sexpr | Sexpr[]];
type Fn = [`fn`, Args, Each | Each[] | Sexpr | Sexpr[]]

type Var = {
  name: string;
  value: string | Atom;
};

type Env = [] | Var[];

type NotMatch = "NotMatch";
const NotMatch = "NotMatch";

// -------------------------

type MakeVar<N, V> = { name: N; value: V };

type GetVar<T, E> = E extends Env
  ? E extends [infer U, ...infer R]
    ? U extends Var
      ? U["name"] extends T
        ? U["value"]
        : GetVar<T, R>
      : NotMatch
    : NotMatch
  : NotMatch;

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
> = NotMatch;

//------------------------------------------------

// Let
// Env in Let must be a list of LIFO.
//
// Note:
// This "Lifo" has no mean but just a naming.
//
// Sym in Sym itself is loadable but ReadLet doesn't do that
// because this case is broken in a macro context.
type EnvLifo = Env[];

type LetError0 = "LetError";
type Let<N, V, EnvLifo = Env[]> = EnvLifo extends Env[]
  ? [...EnvLifo, [MakeVar<N, V>]]
  : LetError0;

type ReadLet<N, EnvLifo = [[]]> = EnvLifo extends [...infer HS, infer L]
  ? L extends Env
    ? GetVar<N, L> extends NotMatch
      ? ReadLet<N, HS>
      : GetVar<N, L>
    : NotMatch
  : NotMatch;

// test
type LetEnvLifo = [
  [
    MakeVar<"ss", "stringer">,
    MakeVar<"s", "string">,
    MakeVar<"cc", [`prim`, "p/cc"]>,
  ],
];
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
const readLetTest3: ReadLet<"ssss", Let<"sss", "str", LetEnvLifo>> = NotMatch;
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
      ? ReadLetRecur<rest, env, [...R, P extends NotMatch ? F : P]>
      : never
    : ReadLetRecur<rest, env, [...R, F extends unknown[] ? ReadLetRecur<F, env, []> : F]>
  : R

const readletrecur_test0: ReadLetRecur<['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]], Let<'b', ['prim', '0010'], Let<'a', ['prim', '0001'], LetEnvLifo>>> = ['fn', [['sym', 'x']], [['sym', '+'], ['prim', '0001'], ['prim', '0010']]]

type ReadAtom<A, EnvLifo = [[]], prev = 0> = A extends [`sym`, infer S]
  ? ReadLet<S, EnvLifo>
  : // this returns prim / fn.
    Eval<A, EnvLifo, [prev]>;
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
    AS,
    EnvLifo = [[]],
    prev = 0,
    R = [],
    IsFn extends boolean = false> =
  R extends Atom[]
    ? AS extends [infer H, ...infer T]
      ? H extends Atom
        ? Reading<T, EnvLifo, prev, [...R, ReadAtom<H, EnvLifo, prev>]>
        : H extends Sexpr
          ? Reading<T, EnvLifo, prev, [...R, Eval<H, EnvLifo, prev>]>
          : { error: [ReadingError1]}
      : R
    : { error: [ReadingError0]; env: EnvLifo };


// test reading
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
> = {error: ['ReadingError0'], env: [[]]}
const readingtest2: Reading<
[['sym', 'a'], ['sym', 'b'], [['sym', 'str'], ['prim', "'s1'"], ['prim', "'s2'"]]],
[[],
 [MakeVar<"a", ['sym', 'str']>, 
  MakeVar<'b', ['prim', "'bs'"]>]]> = [['sym', 'str'], ['prim', "'bs'"], ['prim', "'s1s2'"]]


//-----------------------------------------

// Def
//
// Note: Ignore duplicate def (in current)
//   to fix it, making error handling type.

type Def<N, V, EnvLifo> = EnvLifo extends [infer U, ...infer R]
  ? U extends Env
    ? [[MakeVar<N, V>, ...U], ...R]
    : never
  : never;

// todo : test

//------------------------------------------
// todo :
// write (inc a) in a = 1
// such like <`(inc a)`, LetLifo>
// some needs inc or implemented fn already.
//
// `(inc a)` disassemblied into [inc, a]
//   as a way of used in Elixir (maybe).
// Then eval them.
//
// or because 'inc needs a type level number,
//   peano number,
// it is easier of defining inc to append + into a string.
// ----------------------------------------

type AppendError = "AppendError";
type AppendP<S> = S extends [`prim`, `'${infer U}'`]
  ? [`prim`, `'+${U}'`]
  : { error: [AppendError, S] };

const appendTest: AppendP<[`prim`, "'test'"]> = [`prim`, "'+test'"];

// note:
// if `R extends string` doesn't exist, 
// R cannot be passed into `${R}` because ts can get the type of R.
type StrError0 = "StrError0";
type Str<S, R = ""> = R extends string
  ? S extends [[`prim`, `${infer HS}`], ...infer T]
    ? HS extends `'${infer hs}'` | `'${infer hs}'`
      ? Str<T, `${R}${hs}`>
      : Str<T, `${R}${HS}`>
    : [`prim`, `'${R}'`]
  : StrError0;

// test str
const strtest1: Str<[[`prim`, `test`], [`prim`, `+`], [`prim`, `tail`]]> = [
  `prim`,
  `'test+tail'`,
];
const strtest2: Str<[[`prim`, `test`]]> = [`prim`, `'test'`];



// --------------------------------------------
// -- Logical Operators
// --------------------------------------------

type _And<Fst, Snd> = Fst extends false ? false : Snd extends false ? false : true

type _LispAnd<S> = S extends [infer Fst, ...infer Rest]
  ? Fst extends [`prim`, infer Boolean]
    ? Boolean extends `nil`
      ? false 
      : Rest extends []
        ? Boolean
        : _And<Boolean, _LispAnd<Rest>>
    : never
  : never;

// note : all truthy only excepts false and nil.
type LispAnd<S> = S extends [infer _, ...infer __]
  ? [`prim`, _LispAnd<S>]
  : [`prim`, false];

const lispandtest1: LispAnd<[[`prim`, true], [`prim`, true]]> = [`prim`, true];
const lispandtest2: LispAnd<[[`prim`, true], [`prim`, false]]> = [`prim`, false];
const lispandtest2_1: LispAnd<[[`prim`, false], [`prim`, true]]> = [`prim`, false];
const lispandtest3: LispAnd<[[`prim`, true], [`prim`, false], [`prim`, true]]> =
  [`prim`, false];
const lispandtest3_1: LispAnd<[[`prim`, false], [`prim`, false], [`prim`, true]]> =
  [`prim`, false];
const lispandtest4: LispAnd<[[`prim`, true], [`prim`, true], [`prim`, true]]> =
  [`prim`, true];
const lispandtest5: LispAnd<[[`prim`, true], [`prim`, "nil"]]> = [`prim`, false];
const lispandtest6: LispAnd<[[`prim`, "nil"], [`prim`, "''"]]> = [`prim`, false];
const lispandtest7: LispAnd<[[`prim`, "nil"], [`prim`, "nil"]]> = [`prim`, false];
const lispandtest8: LispAnd<[[`prim`, "nil"], [`prim`, false]]> = [`prim`, false];
const lispandtest9: LispAnd<[[`prim`, false], [`prim`, "nil"]]> = [`prim`, false];

type _Or<Fst, Snd> = Fst extends true ? true : Snd extends true ? true : false

// todo : optimize
type _LispOr<S> = 
S extends []
  ? false
  : S extends [infer Fst, ...infer Rest]
    ? Fst extends [`prim`, false] | [`prim`, 'nil']
      ? _LispOr<Rest>
      : true
    : never

// note : all truthy only excepts false or nil.
type LispOr<S> = S extends [infer _, ...infer __]
  ? [`prim`, _LispOr<S>]
  : [`prim`, false];

const lisportest1: LispOr<[[`prim`, true], [`prim`, true]]> = [`prim`, true];
const lisportest2: LispOr<[[`prim`, true], [`prim`, false]]> = [`prim`, true];
const lisportest2_1: LispOr<[[`prim`, false], [`prim`, true]]> = [`prim`, true];
const lisportest3: LispOr<[[`prim`, true], [`prim`, false], [`prim`, true]]> =
  [`prim`, true];
const lisportest3_1: LispOr<[[`prim`, false], [`prim`, false], [`prim`, true]]> =
  [`prim`, true];
const lisportest4: LispOr<[[`prim`, true], [`prim`, true], [`prim`, true]]> =
  [`prim`, true];
const lisportest5: LispOr<[[`prim`, true], [`prim`, "nil"]]> = [`prim`, true];
const lisportest6: LispOr<[[`prim`, "nil"], [`prim`, "''"]]> = [`prim`, true];
const lisportest7: LispOr<[[`prim`, "nil"], [`prim`, "nil"]]> = [`prim`, false];
const lisportest8: LispOr<[[`prim`, "nil"], [`prim`, false]]> = [`prim`, false];
const lisportest9: LispOr<[[`prim`, false], [`prim`, "nil"]]> = [`prim`, false];

// todo : naming
type _Eq<Fst, Snd> = Fst extends Snd ? (Snd extends Fst ? Fst : never) : never;

type _LispEq<S> = S extends [infer Fst, ...infer Rest]
  ? Rest extends []
    ? Fst
    : _Eq<Fst, _LispEq<Rest>>
  : never;

type LispEq<S> = S extends [infer Fst, ...infer Rest]
  ? [`prim`, Eq<Fst, _LispEq<S>>]
  : [`prim`, false];

// test
const lispeqtest: _Eq<[`prim`, "'a'"], [`prim`, "'a'"]> = [`prim`, "'a'"];
const lispeqtest1: LispEq<[[`prim`, "'a'"], [`prim`, "'a'"]]> = [`prim`, true];
const lispeqtest2: LispEq<[[`prim`, "'a'"], [`prim`, "'b'"]]> = [`prim`, false];
const lispeqtest3: LispEq<[[`prim`, "'a'"], [`prim`, "'b'"], [`prim`, "'a'"]]> =
  [`prim`, false];
const lispeqtest4: LispEq<[[`prim`, "'a'"], [`prim`, "'a'"], [`prim`, "'a'"]]> =
  [`prim`, true];
const lispeqtest5: LispEq<[[`prim`, "'a'"], [`prim`, "''"]]> = [`prim`, false];

type _Not<B> = B extends false ? true : false
type LispNot<S> = S extends [['prim', infer U]] ? ['prim', _Not<U>] : never

// test
const lispnottest0: LispNot<[['prim', false]]> = ['prim', true]
const lispnottest1: LispNot<[['prim', true]]> = ['prim', false]
const lispnottest2: LispNot<[['prim', 1]]> = ['prim', false]

// -------------------------------
// -- Bit Operators
// -------------------------------

type LispAddError0 = 'LispAddError0'
type LispAddError1 = 'LispAddError1'
type LispAdd<
  S
  , R extends string = "00000000"> = 
  S extends []
    ? [`prim`, R]
    : S extends [infer Fst, ...infer Rest]
      ? Fst extends [`prim`, infer FstP extends string]
        ? LispAdd<Rest, Bit.BitAdd<R, FstP>>
        : {error: [LispAddError0]}
      : {error: [LispAddError1]}

const testlispadd0: LispAdd<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, '0000000000000100']
const testlispadd1: LispAdd<[[`prim`, '00000011'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, '0000000000000111']
const testlispadd2: LispAdd<[[`prim`, '00001001'], [`prim`, '00000110'], [`prim`, '00000001']]> = [`prim`, '0000000000010000']


34
type LispSubError0 = 'LispSubError0'
type LispSub<
  S
  , R extends string = "00000000"
  , Init extends boolean = true> = 
  S extends []  
    ? [`prim`, R]
    : S extends [[`prim`, infer Fst extends string], ...infer Rest extends string[][]]
      ? Init extends true
        ? LispSub<Rest, Fst, false>
        : LispSub<Rest, Bit.BitSub<R,Fst>, false>
      : {error: [LispSubError0]}

const testlispsub0: LispSub<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, '0000000000000010']
const testlispsub1: LispSub<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, '0000000000001011']

type LispMulError0 = 'LispMulError0'
type LispMul<
  S
  , R extends string = "00000000"
  , Init extends boolean = true> = 
  S extends []
    ? [`prim`, R]
    : S extends [[`prim`, infer Fst extends string], ...infer Rest extends string[][]]
      ? Init extends true
        ? LispMul<Rest, Fst, false>
        : LispMul<Rest, Bit.BitMul<R,Fst>, false>
      : {error: [LispMulError0]}

const testlispmul0: LispMul<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, '0000000000000011']
const testlispmul1: LispMul<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, '0000000000101101']

type LispDivError0 = 'LispDivError0'
type LispDivError1 = 'LispDivError1'
type LispDiv<
  S
  , R extends string = "00000001"
  , Init extends boolean = true> = 
  S extends []
    ? [`prim`, R]
    : S extends [[`prim`, infer Fst extends string], ...infer Rest extends string[][]]
      ? Init extends true
        ? LispDiv<Rest, Fst, false>
        : Bit.BitDiv<R,Fst> extends Bit.Nil | string & infer Div
            // ----------------------------
            // ? Div extends Bit.Nil
            //   ? Bit.Nil
            //   : LispDiv<Rest, Div, false>
            // : never 
            // -----------------------------
            // note : this is ts error 2344, what?
            // -----------------------------
            ? Div extends string
              ? LispDiv<Rest, Div, false>
              : Bit.Nil
            : {error: [LispDivError0]}
          : {error: [LispDivError1]}

const testlispdiv0: LispDiv<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, '0000000000000011']
const testlispdiv1: LispDiv<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, '0000000000000101']
const testlispdiv2: LispDiv<[[`prim`, '00000011'], [`prim`, '0000000']]> = [`prim`, 'nil']

type LispModError0 = 'LispModError0'
type LispModError1 = 'LispModError1'
type LispMod<
  S
  , R extends string = "0000000000000000"
  , Init extends boolean = true> = 
  S extends []
    ? [`prim`, R]
    : S extends [[`prim`, infer Fst extends string], ...infer Rest extends string[][]]
      ? Init extends true
        ? Bit.BitGTE<Fst, '0000000000000000'> extends true
          ? LispMod<Rest, Fst, false>
          : Rest extends [['prim', infer Snd extends string]]
            ? LispMod<[['prim', Bit.BitSub<Snd, Bit.BitRevSign<Fst>>], ['prim', Snd]]>
            : never // 
        : Bit.BitMod<R,Fst> extends Bit.Nil | string & infer Mod
            ? Mod extends string
              ? LispMod<Rest, Mod, false>
              : Bit.Nil
            : {error: [LispModError0]}
          : {error: [LispModError1]}

const testlispmod0: LispMod<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, '0000000000000000']
const testlispmod1: LispMod<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, '0000000000000000']
const testlispmod2: LispMod<[[`prim`, '00000011'], [`prim`, '0000000']]> = [`prim`, 'nil']
const testlispmod3: LispMod<[[`prim`, '00000101'], [`prim`, '0000010']]> = [`prim`, '0000000000000001']
const testlispmod4: LispMod<[[`prim`, '00010001'], [`prim`, '00000011']]> = [`prim`, '0000000000000010']
const testlispmod5: LispMod<[['prim', '1111111111111110'], ['prim', '0000000000000101']]> = ['prim', '0000000000000011']
const testlispmod6: LispMod<[['prim', '1111111111101111'], ['prim', '0000000000000101']]> = ['prim', '0000000000000011']

type LispRelationError0 = 'LispRelationError0'
type LispRelationError1 = 'LispRelationError1'
type LispRelation<
  Name extends string
  , S
  , R extends string = "00000000"
  , Init extends boolean = true
  , Next extends boolean = true> = 
  S extends []
    // note :
    // a bit pitfall, this shouln't be [`prim`, true]
    // see in the case of '>' and the args are two.
    ? [`prim`, Next]
    : S extends [[`prim`, infer Fst extends string], ...infer Rest extends string[][]]
      ? Init extends true
        ? LispRelation<Name, Rest, Fst, false, Next>
        : Next extends true
          ? Name extends '>'
            ? LispRelation<Name, Rest, Fst, false, Bit.BitGT<R,Fst>>
          : Name extends '<'
            ? LispRelation<Name, Rest, Fst, false, Bit.BitLT<R,Fst>>
          : Name extends '>='
            ? LispRelation<Name, Rest, Fst, false, Bit.BitGTE<R,Fst>>
          : Name extends '<='
            ? LispRelation<Name, Rest, Fst, false, Bit.BitLTE<R,Fst>>
            : {error: [LispRelationError0]}
          : [`prim`, false]
      : {error: [LispRelationError1]}
 
const testlispgt0: LispRelation<'>',  [[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, true]
const testlispgt1: LispRelation<'>',  [[`prim`, '00001111'], [`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, true]
const testlispgt2: LispRelation<'>',  [[`prim`, '00001111'], [`prim`, '00001111'], [`prim`, '00001111']]> = [`prim`, false]

const testlisplt0: LispRelation<'<',  [[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, false]
const testlisplt1: LispRelation<'<',  [[`prim`, '00001111'], [`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, false]
const testlisplt2: LispRelation<'<',  [[`prim`, '00001111'], [`prim`, '00001111'], [`prim`, '00001111']]> = [`prim`, false]

const testlispgte0: LispRelation<'>=',  [[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, true]
const testlispgte1: LispRelation<'>=',  [[`prim`, '00001111'], [`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, true]
const testlispgte2: LispRelation<'<=',  [[`prim`, '00001111'], [`prim`, '00001111'], [`prim`, '00001111']]> = [`prim`, true]

const testlisplte0: LispRelation<'<=',  [[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, false]
const testlisplte1: LispRelation<'<=',  [[`prim`, '00001111'], [`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, false]
const testlisplte2: LispRelation<'<=',  [[`prim`, '00001111'], [`prim`, '00001111'], [`prim`, '00001111']]> = [`prim`, true]

// -------------------------------------------

type EvalError1 = "EvalError1";
type EvalError2 = "EvalError2";
type EvalError3 = "EvalError3";
type EvalError4 = "EvalError4";
type EvalError5 = "EvalError5";
type EvalError6 = "EvalError6";
type EvalError7 =
  "EvalError7/ 2nd in let form must be Atom. No wrapped value is deprecated.";
type EvalError8 = "EvalError8";
type EvalError9 = "EvalError9";
type EvalError10 = "EvalError10";
type EvalError11 =
  "EvalError11/ Some of elements type in SEXPR doesn't satisfy EACH.";
type EvalError12 = "EvalError12";
type EvalError13 = "EvalError13"
type EvalError14 = "EvalError14"
type EvalError15 = "EvalError15"
type EvalError16 = "EvalError16"
type EvalError17 = "EvalError17"
type EvalError18 = "EvalError18"
type EvalError19 = "EvalError19"
type EvalError20 = "EvalError20"

// -----------------
// memo
// type arrr = [number, ...number[]]
// const aaaaaa: arrr = [1,2,3,4]
// ------------------

// test fn
const dectest: Fn = [
  `fn`,
  [[`sym`, `a`]],
  [
    [`sym`, `a`],
    [`sym`, `b`],
  ],
];

// -------------------------------------
type Eq<L, R> = L extends R ? (R extends L ? true : false) : false;
// type Not<L,R> L extends
type If<A, B, C> = A extends [`prim`, true] ? B : C;
type IfForm = [`if`, Each | Sexpr, Each | Sexpr, Each | Sexpr];

const eqtest1: Eq<"a", "a"> = true;
const eqtest2: Eq<"a", ""> = false;
const eqtest3: Eq<null, []> = false;
const eqtest4: Eq<undefined, null> = false;
const eqtest5: Eq<undefined, undefined> = true;
// biome-ignore lint/complexity/noBannedTypes:
const eqtest6: Eq<{}, null> = false;
const eqtest7: Eq<1, "1"> = false;
const eqtest8: Eq<["a"], ["a", ""]> = false;
const eqtest9: Eq<[""], ["a"]> = false;
const eqtest10: Eq<["a"], ["a"]> = true;
const eqtest11: Eq<[""], [""]> = true;

// ----------------------------------
// vector/list/array
type Vector = [`vec`, ...Atom[]] | [`vec`];
const vectortest: Vector & [`vec`, [`prim`, `1`]] = [`vec`, [`prim`, `1`]];
const vectortest2: Vector & [`vec`, [`prim`, `1`], [`prim`, `2`]] = [
  `vec`,

  [`prim`, `1`],
  [`prim`, `2`],
];
const vectortest3: Vector & [`vec`, [`vec`, [`prim`, `2`]]] = [
  `vec`,
  [`vec`, [`prim`, `2`]],
];
// error TS2322: Type '"prim"' is not assignable to type '"vec"'.
// const vectortest4: Vector = [`prim`, `1`]
const vectortest5: Vector &
  [`vec`, [`vec`, [`vec`, [`prim`, true], [`prim`, `1`]], [`prim`, `1`]]] = [
  `vec`,
  [`vec`, [`vec`, [`prim`, true], [`prim`, `1`]], [`prim`, `1`]],
];

type GetVecError0 = "GetVecError0";
type GetVecError1 = "GetVecError1";
type GetVecError2 = "GetVecError2";
type GetVec<N, V> = N extends number
  ? V extends Vector & [`vec`, ...infer W]
    ? W extends Atom[]
      ? W[N]
      : GetVecError0
    : GetVecError1
  : GetVecError2;
const testgetvec: GetVec<
  3,
  [`vec`, [`prim`, 0], [`prim`, 1], [`prim`, 2], [`prim`, 3], [`prim`, 4]]
> = [`prim`, 3];

// map


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

type LispConcat<S, R extends unknown[][] = []> =
  S extends Vector[] & [['vec', ...infer H], ...infer T]
    ? T extends []
      ? TConcat<R>
      : LispConcat<T, [...R, H]>
    : ConcatError0

const tconcattest0: TConcat<[[0,1], [2,3], [4,5]]> = [0,1,2,3,4,5]

const ttm: TMap = ['map', [['key', ':b'], ['key', ':b']]]


type IsKeyword<T> = 
  T extends ['key', `:${infer S}`]
  ? true
  : false

type IsMap<T> = T extends TMap ? true : false

type IsKeyMapSexpr<S> = 
  S extends [infer Fst, infer Snd]
    ? IsKeyword<Fst> extends true
      ? IsMap<Snd> extends true
        ? true
        : false
      : IsKeyword<Snd> extends true
        ? IsMap<Fst> extends true
          ? true
          : false
        : false
    : false

const iskeymapsexprtest0: IsKeyMapSexpr<[['key', ':a'],['key', ':b']]> = false
const iskeymapsexprtest1: IsKeyMapSexpr<[['map', [['key', ':a'], ['prim', '0']]],['map', [['key', ':a'], ['prim', '1']]]]> = false
const iskeymapsexprtest2: IsKeyMapSexpr<[['key', ':a'],['map', [['key', ':a'], ['prim', '0']]]]> = true
const iskeymapsexprtest3: IsKeyMapSexpr<[['map', [['key', ':a'], ['prim', '0']]],['key', ':a']]> = true

type GetMapError0 = "GetMapError0";
type GetMapError1 = "GetMapError1";
type GetMapError2 = "GetMapError2";

type GetMap<K,V,sV = V extends [infer _, infer i] ? i : never> = 
  sV extends [infer k, infer v, ... infer _]
  ? k extends K
    ? v
    : GetMap<K,V,sV extends [infer _, infer __, ...infer i] ? i : never>
  : Nil

const testgetmap0: GetMap<['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]> = ['prim', '0']
const testgetmap1: GetMap<['key', ':a'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]> = ['prim', '0']
const testgetmap2: GetMap<['key', ':c'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]> = Nil

type Get<K, V> = V extends Vector ? GetVec<K, V> : GetMap<K, V>;
const testget0: Get<
  3,
  [`vec`, [`prim`, 0], [`prim`, 1], [`prim`, 2], [`prim`, 3], [`prim`, 4]]
> = [`prim`, 3];
const testget1: Get<['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]> = ['prim', '0']
const testget2: Get<['key', ':a'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]> = ['prim', '0']
const testget3: Get<['key', ':c'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]> = Nil

type LispGetError0 = "LispGetError0"
type LispGet<S> =
    S extends [infer Map extends TMap, infer Key extends Keyword]
      ? Get<Key, Map>
      : S extends [infer Vec extends Vector, infer Idx extends ['prim', string]]
        ? Get<Idx, Vec>
        : {error: [LispGetError0, S, "this is not map and key or vector and idx-num."]
           sexpr: S}

type LispVectorError0 = "LispVectorError0"
type LispVector<S> = S extends unknown[] ? ['vec', ...S] : {error: [LispGetError0, S, ""], sexpr: S}

// fns of seq
type FirstError0 = "FirstError0";
type FirstError1 = "FirstError1";
type RestError0 = "RestError0";
type RestError1 = "RestError1"

type ConcatError = "ConcatError";

type First<V> = V extends Vector & [`vec`, infer H, ...infer T]
  ? H
  : V extends ['vec']
    ? Nil
    : FirstError0;
type LispFirst<S> = S extends [infer V extends Vector] ? First<V> : FirstError1

type LastError0 = 'LastError0'
type LastError1 = 'LastError1'
type Last<V> =
  V extends [infer H, ...infer T]
    ? T extends []
      ? H
      : Last<T>
    : LastError0
type LispLast<S> = S extends [['vec']] ? Nil : S extends [['vec', ...infer V]] ? Last<V> : LastError1

type Rest<V> = V extends Vector & [`vec`, infer H, ...infer T]
  ? T[0] extends Atom
    ? [`vec`, ...T]
    : [`vec`]
  : RestError0;
type LispRest<S> = 
  S extends [infer V extends Vector] ? Rest<V> : RestError1

type ButlastError0 = "ButlastError0"
type ButlastError1 = "ButlastError1"
type ButlastError2 = "ButlastError2"
type ButlastError3 = "ButlastError3"
type _Butlast<V, R extends unknown[] = []> = 
  V extends [infer H, ...infer T]
    ? T extends []
      ? R
      : _Butlast<T,[...R,H]>
    : ButlastError0
type Butlast<V> = V extends Vector & ['vec', ...infer v] ? _Butlast<v> extends Atom[] & infer a ? ['vec', ..._Butlast<v>] : ButlastError1 : ButlastError3
type LispButlast<S> = S extends [infer V extends Vector] ? Butlast<V> : ButlastError2
type testbutlastvec = [`vec`, [`prim`, 0], [`prim`, 1], ['prim', 2], ['prim', 3]]
const testbutlast0: Butlast<testbutlastvec> = [`vec`, [`prim`, 0], [`prim`, 1], ['prim', 2]];
const testbutlast1: Butlast<Butlast<testbutlastvec>> = [`vec`, [`prim`, 0], [`prim`, 1]];
const testbutlast2: Butlast<Butlast<Butlast<testbutlastvec>>> = [`vec`, [`prim`, 0]];
const testbutlast3: Butlast<Butlast<Butlast<Butlast<testbutlastvec>>>> = [`vec`];

type ConjError0 = "ConjError0"
type ConjError1 = "ConjError1"
type ConjError2 = "ConjError2"
type ConjError3 = "ConjError3"
type Conj<V, E> = E extends Atom
  ? V extends Vector
    ? [...V, E]
    : ConjError0
  : ConjError0;
type LispConj<S> =
  S extends [infer H extends Vector, ...infer T extends Atom[]]
    ? [...H, ...T]
    : ConjError2

type Concat<V, W> = V extends Vector
  ? W extends Vector & [`vec`, ...infer WW]
    ? [...V, ...WW]
    : ConcatError
  : ConcatError;
// get, assoc, update
// type Get<S, K> = S extends Vector & [`vec`, infer V] ? K extends number ? V[K] : never : S extends HashMap & [`HashMap`, infer V] ? K extends string ? V[K] : never : never

type testvec = [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1]];
const testfirst: First<testvec> = [`prim`, true];
const testrest: Rest<testvec> = [`vec`, [`prim`, 0], [`prim`, 1]];
const testrest1: Rest<Rest<testvec>> = [`vec`, [`prim`, 1]];
const testrest2: Rest<Rest<Rest<testvec>>> = [`vec`];
const testconj: Conj<testvec, [`prim`, false]> = [
  `vec`,
  [`prim`, true],
  [`prim`, 0],
  [`prim`, 1],
  [`prim`, false],
];
const testconj1: Conj<[`vec`], [`prim`, false]> = [`vec`, [`prim`, false]];
const testconcat: Concat<testvec, testvec> = [
  `vec`,
  [`prim`, true],
  [`prim`, 0],
  [`prim`, 1],
  [`prim`, true],
  [`prim`, 0],
  [`prim`, 1],
];

type TakeError0 = "TakeError0"
type TakeError1 = "TakeError1"
type TakeError2 = "TakeError2"
type TakeError3 = "TakeError3"
type Take<N extends string, V extends unknown[], R extends unknown[] = []> =
  V extends []
    ? R
    : Bit.BitGTE<"0", N> extends true
      ? R
      : V extends [infer F, ...infer T]
        ? Take<Bit.BitSub<N, "1">, T, [...R, F]>
        : TakeError0
type LispTake<S> =
    S extends [['prim', infer N extends string], ['vec', ...infer V]]
      ? Take<N,V> extends infer RV
        ? RV extends unknown[]
          ? ['vec', ...RV]
          : TakeError2
        : TakeError3
      : TakeError1

const testtakem0: Bit.BitSub<"11", "1"> = "0000000000000010"
const testtakem1: Bit.BitIsZero<"0"> = true
const testtake0: Take<"11", [0,1,2,3,4,5,6]> = [0,1,2]


type DropError0 = "DropError0"
type DropError1 = "DropError1"
type DropError2 = "DropError2"
type DropError3 = "DropError3"
type Drop<N extends string, V extends unknown[], R extends unknown[] = []> =
  V extends []
    ? R
    : V extends [infer _, ...infer T]
      ? Bit.BitGTE<"0", N> extends true
        ? V
        : Drop<Bit.BitSub<N, "1">, T>
      : DropError0
type LispDrop<S> =
    S extends [['prim', infer N extends string], ['vec', ...infer V]]
      ? Drop<N,V> extends infer RV
        ? RV extends unknown[]
          ? ['vec', ...RV]
          : DropError2
        : DropError3
      : DropError1
const testDropm0: Bit.BitSub<"11", "1"> = "0000000000000010"
const testDropm1: Bit.BitIsZero<"0"> = true
const testDrop0: Drop<"11", [0,1,2,3,4,5,6]> = [3,4,5,6]

// map, filter, remove, every, some
type FMapError = "MapError";
type FilterError = "FilterError";
type RemoveError = "RemoveError";
type EveryError = "EveryError";
type SomeError = "SomeError";
type _FMap<F, V, Env = [[]], prev = [0]> = V extends Vector
  ? V extends [`vec`, infer H, ...infer T]
    ? T[0] extends Atom
      ? [Eval<[F, H]>, ..._FMap<F, [`vec`, ...T]>]
      : [Eval<[F, H]>]
    : [0]
  : [1];
type FMap<F, V, Env = [[]], prev = [0]> = [`vec`, ..._FMap<F, V>];
type LispMap<S> =
  S extends [infer f, infer vs]
    ? FMap<f, vs> 
    : FMapError 
// test fmaps
type testf = Sym & [`sym`, `AppendP`];

// if not directly input those sexpr, through args, this fmap eval returns any, because of ...infer T (in FMap) would be expanded unknown.
const testargv = [`vec`, [`prim`, `'1'`], [`prim`, `'2'`]];
const testfmap: FMap<
  [`sym`, `AppendP`],
  [`vec`, [`prim`, `'1'`], [`prim`, `'2'`]]
> = [`vec`, [`prim`, `'+1'`], [`prim`, `'+2'`]];

type _Filter<F, V, Env = [[]], prev = [0]> = V extends Vector
  ? V extends [`vec`, infer H, ...infer T]
    ? T[0] extends Atom
      ? Eval<[F, H]> extends [`prim`, true]
        ? [H, ..._Filter<F, [`vec`, ...T]>]
        : [..._Filter<F, [`vec`, ...T]>]
      : Eval<[F, H]> extends [`prim`, true]
        ? [H]
        : []
    : ["notvecatom"]
  : ["notvec"];

type Filter<F, V, Env = [[]], prev = [0]> = [`vec`, ..._Filter<F, V>];
type LispFilter<S> =
  S extends [infer f, infer vs]
    ? Filter<f, vs> 
    : FilterError 
type LispRemove<S> =
  S extends [infer f, infer vs]
    ? Filter<['fn', [['sym', 'aaa']], [['sym', 'not'], [f, ['sym', 'aaa']]]], vs>
    : FilterError  

const jkkjkt: Eval<[['sym', 'not'], [['sym', '>'], ['prim', '0000000000000010'], ['prim', '00000000000000001']]]> = ['prim', false]
const testakj: Sexpr = [['fn', [['sym', 'aaa']], [['sym', 'not'], [['sym', '>'], ['prim', '0000000000000010'], ['sym', 'aaa']]]], ['prim', '0000000000000010']]
const jktejkst: Sexpr | LetForm = ['let', [['sym', 'aaa'], ['prim', '0000000000000010']], [['sym', 'not'], [['sym', '>'], ['prim', '0000000000000010'], ['sym', 'aaa']]]]
const jltesta: Eval<[['fn', [['sym', 'aaa']], [['sym', 'not'], [['sym', '>'], ['prim', '0000000000000010'], ['sym', 'aaa']]]], ['prim', '0000000000000010']]> = ['prim', true]

const testfilter0: Filter<
  [`fn`, [[`sym`, `a`]], [[`sym`, `eq`], [`sym`, `a`], [`prim`, 1]]],
  [`vec`, [`prim`, 0], [`prim`, 1], [`prim`, 1], [`prim`, 2]]
> = [`vec`, [`prim`, 1], [`prim`, 1]];

// const testfilter1: Filter<
//   [`fn`, [[`sym`, `a`]], [['sym', 'not'], [[`sym`, `eq`], [`sym`, `a`], [`prim`, 1]]]],
//   [`vec`, [`prim`, 0], [`prim`, 1], [`prim`, 1], [`prim`, 2]]
// > = [`vec`, [`prim`, 1], [`prim`, 1]];

const testfnlispeqa: Eval<[[`sym`, `eq`], [`prim`, 0], [`prim`, 1]]> = [
  `prim`,
  false,
];
const testfnlispeqaa: Eval<
  [[`sym`, `eq`], [`sym`, `a`], [`prim`, 1]],
  [[MakeVar<`a`, [`prim`, 0]>]]
> = [`prim`, false];

const testfnlispeq0: Eval<
  [
    [`fn`, [[`sym`, `a`]], [[`sym`, `eq`], [`sym`, `a`], [`prim`, `'1'`]]],
    [`prim`, `'0'`],
  ]
> = [`prim`, false]; // todo : fix

const testfnlispeq1: Eval<
  [
    [`fn`, [[`sym`, `a`]], [[`sym`, `eq`], [`sym`, `a`], [`prim`, `'1'`]]],
    [`prim`, `'1'`],
  ]
> = [`prim`, true]; // todo : fix

// const evalfntest2: Eval<
//   [[`fn`, [[`sym`, `str`]], Tdddd], [`prim`, `'test'`]],
//   [[MakeVar<"aaa", "'aaa'">], [MakeVar<"str", "'strval'">]]
// > = [`prim`, "'+test'"];

// todo : error handle properly
type InterleaveError0 = "InterleaveError0";
type InterleaveError1 = "InterleaveError1";
type Interleave<V, W> = V extends [infer HeadV, ...infer TailV]
  ? W extends [infer HeadW, ...infer TailW]
    ? TailW extends never
      ? []
      : TailV extends never
        ? []
        : [HeadV, HeadW, ...Interleave<TailV, TailW>]
    : []
  : [];
type LispInterleave<S> = S extends [['vec', ...infer V], ['vec', ...infer W]] ? ['vec', ...Interleave<V, W>] : InterleaveError1

// interleave test
const testinterleave0: Interleave<[1, 2, 3], [4, 5, 6]> = [1, 4, 2, 5, 3, 6];
const testinterleave1: Interleave<[1, 2, 3], [4, 5]> = [1, 4, 2, 5];
const testinterleave2: Interleave<[1], [2]> = [1, 2];

type ReduceError0 = 'ReduceError0'
type ReduceError1 = 'ReduceError1'
type ReduceError2 = 'ReduceError2'
type _Reduce<F, Init, V> =
  V extends [infer H, ...infer T]
  ? T['length'] extends 0
    ? Eval<[F, Init, H]>
    : _Reduce<F, Eval<[F, Init, H]>, T>
  : {error: [ReduceError0]}
type Reduce<F,Init,V> =
  V extends ['vec', ...infer v]
    ? _Reduce<F,Init,v>
    : {error: [ReduceError1]}
type LispReduce<S> = 
  S extends [infer f, infer init, infer v]
    ? Reduce<f,init,v>
    : ReduceError2

const testreduce0: Reduce<
    ['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]],
    ['prim', '0'],
    ['vec', ['prim', '01'], ['prim', '10']]
> = ['prim', '0000000000000011']
const testttt0: Eval<[['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]]]> = {error: ['LispAddError1']}
const testttt1: Eval<['let', [], ['prim', '1']]> = ['prim', '1']
const testttt2: Eval<[['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]], ['prim', '10'], ['prim', '11']]> = ['prim', '0000000000000101']


type ReverseError0 = 'ReverseError0'
type ReverseError1 = 'ReverseError1'
type ReverseError2 = 'ReverseError2'
type ReverseError3 = 'ReverseError3'
type Reverse<V, R extends Array<unknown> = []> = 
  V extends [infer H, ...infer T]
  ? T['length'] extends 0
    ? [H, ...R]
    : Reverse<T, [H, ...R]>
  : V extends []
    ? []
    : {error: [ReverseError0]}
type LispReverse<S> = S extends [Vector] & [['vec', ...infer V]] ? Reverse<V> extends infer RV ? RV extends unknown[] ? ['vec', ...RV] : ReverseError1 : ReverseError2 : ReverseError3
const reversetest0: Reverse<[0,1,2,3,4]> = [4,3,2,1,0]
const reversetest1: Reverse<[]> = []
const reversetest2: Reverse<[1]> = [1]

// note : for threading macros: insertsecond, insertlast, vecwrap
type InsertSecondError0 = 'InsertSecondError0'
type InsertSecondError1 = 'InsertSecondError1'
type InsertSecond<V, E> = 
  V extends [infer H, ...infer R]
    ? [H, E, ...R]
    : V extends [...infer R]
      ? [E, ...R]
      : {error: [InsertSecondError0]}

const insert2ndtest0: InsertSecond<[0,1,2,3], 'x'> = [0,'x',1,2,3]
const insert2ndtest1: InsertSecond<[0], 'x'> = [0,'x']
const insert2ndtest2: InsertSecond<[], 'x'> = ['x']

type InsertLastError0 = 'InsertLastError0'
type InsertLast<V, E> = 
  V extends [...infer R]
  ? [...R, E]
  : {error: [InsertLastError0]}
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
type ThreadFirst<
Fst
, V extends unknown[]
, R extends unknown[] = []
, Init extends boolean = true
> =
  V['length'] extends 0
    ? InsertSecond<VecWrap<Fst>, R> 
    : V extends [infer Head, ...infer Tail]
      ? Init extends false 
        ? ThreadFirst<Head, Tail, InsertSecond<VecWrap<Fst>, R>, false>
        : Tail extends [infer N, ...infer M]
          ? ThreadFirst<N, M, InsertSecond<VecWrap<Head>, Fst>, false>
          : InsertSecond<VecWrap<Head>, Fst>
      : {error: [ThreadFirstError1]}
const threadfirsttest0: ThreadFirst<[0], [[[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]]]]]
const threadfirsttest1: ThreadFirst<[0], [[[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]], [22]]]]
const threadfirsttest2: ThreadFirst<[0], [[[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [[1], [0]], [22]]]]
const threadfirsttest3: ThreadFirst<[[0]], [[[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]]]]]
const threadfirsttest4: ThreadFirst<[[0]], [[[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]], [22]]]]
const threadfirsttest5: ThreadFirst<[[0]], [[[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]], [22]]]]
const threadfirsttest6: ThreadFirst<[0], [[1]]> = [[1], [0]]

type LispThreadFirstError0 = 'LispThreadFirstError0'
type LispThreadFirst<V> =
  V extends [infer H, ...infer T]
    ? T['length'] extends 0
      ? V
      : ThreadFirst<H,T>
    : {error: [LispThreadFirstError0]}
const lispthreadfirsttest0: LispThreadFirst<[[0], [[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]]]]]
const lispthreadfirsttest1: LispThreadFirst<[[0], [[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]], [22]]]]
const lispthreadfirsttest2: LispThreadFirst<[[0], [[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [[1], [0]], [22]]]]
const lispthreadfirsttest3: LispThreadFirst<[[[0]], [[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]]]]]
const lispthreadfirsttest4: LispThreadFirst<[[[0]], [[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]], [22]]]]
const lispthreadfirsttest5: LispThreadFirst<[[[0]], [[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]], [22]]]]
const lispthreadfirsttest6: LispThreadFirst<[[0], [1]]> = [[1], [0]]
  


type ThreadLastError0 = 'ThreadLastError0'
type ThreadLastError1 = 'ThreadLastError1'
type ThreadLastError2 = 'ThreadLastError2'
type ThreadLastError3 = 'ThreadLastError3'
type ThreadLast<
Fst
, V extends unknown[]
, R extends unknown[] = []
, Init extends boolean = true
> =
  V['length'] extends 0
    ? InsertLast<VecWrap<Fst>, R> 
    : V extends [infer Head, ...infer Tail]
      ? Init extends false 
        ? ThreadLast<Head, Tail, InsertLast<VecWrap<Fst>, R>, false>
        : Tail extends [infer N, ...infer M]
          ? ThreadLast<N, M, InsertLast<VecWrap<Head>, Fst>, false>
          : InsertLast<VecWrap<Head>, Fst>
      : {error: [ThreadLastError1]}
const threadlasttest0: ThreadLast<[0], [[[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]]]]]
const threadlasttest1: ThreadLast<[0], [[[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [22],[[1],[0]]]]]
const threadlasttest2: ThreadLast<[0], [[[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [22], [[1], [0]]]]]
const threadlasttest3: ThreadLast<[[0]], [[[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]]]]]
const threadlasttest4: ThreadLast<[[0]], [[[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2],[22],[[1], [[0]]]]]]
const threadlasttest5: ThreadLast<[[0]], [[[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2],[22], [[1], [[0]]]]]]

type LispThreadLastError0 = 'LispThreadLastError0'
type LispThreadLast<V> =
  V extends [infer H, ...infer T]
    ? T['length'] extends 0
      ? V
      : ThreadLast<H,T>
    : {error: [LispThreadLastError0]}
const lispthreadlasttest0: LispThreadLast<[[0], [[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [0]]]]]
const lispthreadlasttest1: LispThreadLast<[[0], [[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [22], [[1], [0]]]]]
const lispthreadlasttest2: LispThreadLast<[[0], [[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [22],  [[1], [0]]]]]
const lispthreadlasttest3: LispThreadLast<[[[0]], [[1]], [[2]], [[3]], [[4]]]> = [[4], [[3], [[2], [[1], [[0]]]]]]
const lispthreadlasttest4: LispThreadLast<[[[0]], [[1]], [[2], [22]], [[3]], [[4]]]> = [[4], [[3], [[2], [22], [[1], [[0]]]]]]
const lispthreadlasttest5: LispThreadLast<[[[0]], [[1]], [[2], [22]], [3], [[4]]]> = [[4], [[3], [[2], [22], [[1], [[0]]]]]]
const lispthreadlasttest6: LispThreadLast<[[0], [1]]> = [[1], [0]]
  

// multiarg fn test
const testmultiargfn0: Eval<
  [
    [
      `fn`,
      [[`sym`, `a`], [`sym`, `b`]],
      [[`sym`, `str`], [`sym`, `a`], [`sym`, `b`]],
    ],
    [`prim`, `'0'`],
    [`prim`, `'1'`],
  ]
> = [`prim`, `'01'`];







// ---------------------------------------
// -- Eval
// ---------------------------------------



// todo : ugly
type Eval<A, env = [[]], prev = 0> = A extends Sexpr
  ? A extends [infer OPC, ...infer OPR]
    ? env extends EnvLifo
      ? OPC extends Fn & [`fn`, infer syms, infer D]
        ? Eval<[`let`, Interleave<syms, OPR>, D], env, [prev]>
        // ? Eval<[`let`, Interleave<syms, OPR>, D], env, [prev]>
        : /*
      ? OPC extends Fn & [`fn`, [[`sym`, infer S]], infer D]
        ? OPR[0] extends Sym & [`sym`, infer VV]
          ? // here is a buggy
            // because S of [`sym`, infer S]
            // would be a built-in, then spit an error.
            //
            // should it be expanded to let form
            // if fn has following contents?
            //
            // ? Eval<D, Let<S, ReadLet<VV, env>, env>, [prev]>

            // todo : refactoring
            //   I want to expand fn to let form and omit Let
            //   by hand.
            // to do so, let form should be able to catch
            //   this pattern:
            //   (let [[a b c] [1 2 3]] ...)
            // or reform fn form to:
            //   (let [a 1] ((fn [b] (eq a b)) 2))
            //   from:
            //   ((fn [a b] (eq a b)) 1 2)


            VV extends "AppendP" | "eq" | "str"
            ? Eval<[OPC, Eval<D, env, [prev]>]>
            : Eval<D, Let<S, ReadLet<VV, env>, env>, [prev]>
          : OPR[0] extends Prim & [`prim`, infer VVV]
            ? Eval<D, Let<S, [`prim`, VVV], env>, [prev]>
            : EvalError5
*/
          OPC extends IfForm & [`if`, infer IFCond, infer IFT, infer IFF]
          ? Eval< // point (A)
              [If<Eval<IFCond, env, [[prev]]>, IFT, IFF>, OPR[0]],
              env,
              [prev]
            >
          : OPC extends Sym & [`sym`, infer U]
            ? // care of double-booking.
              ReadLet<U, env> extends NotMatch // `AppendP` | `str`
              // todo : remove and replate this.
              ? U extends `AppendP`
                ? AppendP<ReadAtom<Eval<OPR[0], env, [[prev]]>, env, [prev]>>
              // threading macro: ->, ->>
              // note : place it here
              //   because a number of macros will be up.
                : U extends '->'
                  ? Eval<LispThreadFirst<OPR>, env, [[prev]]>
                : U extends '->>'
                  ? Eval<LispThreadLast<OPR>, env, [[prev]]>
              // note : built-in functions
                : U extends `str`
                  ? Str<Reading<OPR, env, [[prev]]>>
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

                : U extends `concat`
                  ? LispConcat<Reading<OPR, env, [[prev]]>>
                : U extends `conj`
                  ? LispConj<Reading<OPR, env, [[prev]]>>
                : U extends `first`
                  ? LispFirst<Reading<OPR, env, [[prev]]>>
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

                // : U extends `assoc-in`
                //   ? LispReduce<Reading<OPR, env, [[prev]]>>
                // : U extends `update-in`
                //   ? LispReduce<Reading<OPR, env, [[prev]]>>
                // : U extends `assoc`
                //   ? LispReduce<Reading<OPR, env, [[prev]]>>
                // : U extends `update`
                //   ? LispReduce<Reading<OPR, env, [[prev]]>>

                : U extends `get`
                  ? LispGet<Reading<OPR, env, [[prev]]>>
                : U extends `eq` | `=`
                  ? LispEq<Reading<OPR, env, [[prev]]>>
                : U extends `not`
                  ? LispNot<Reading<OPR, env, [[prev]]>>
                : U extends `and`
                  ? LispAnd<Reading<OPR, env, [[prev]]>>
                : U extends `or`
                  ? LispOr<Reading<OPR, env, [[prev]]>> 
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
                : Eval<[ReadLet<U, env>, OPR[0]], env, [prev]>
              : ReadLet<U, env> extends Fn & infer UU
                ? Eval<[UU, ...OPR], env, [prev]>
                // ? Eval<[UU, OPR[0]], env, [prev]>
                : {error: [EvalError3, 'the 1st is not a function but it should be.']
                   env: env}
          // note : (:key map) and (map :key)
          : IsKeyMapSexpr<A> extends true
            ? IsKeyword<OPC> extends true
              ? LispGet<Reading<[...OPR, OPC], env, [[prev]]>>
              : IsKeyword<OPR[0]> extends true
                ? LispGet<Reading<[OPC, ...OPR], env, [[prev]]>>
                : { error: [EvalError12
			   , 'the 1st and 2nd is not keyword.']
		    , env: env
		    , sexpr: A}
          : { error: [EvalError4, 'the 1st is not a symbol but it should be.']
	      , env: env
	      , sexpr: A}
        : { error: [EvalError6, "env 1st shouldn't be [].", prev, A]
	    , env: env
	    , sexpr: A}
    : EvalError2
  : A extends IfForm & [`if`, infer IFCond, infer IFT, infer IFF]
    ? Eval<If<Eval<IFCond, env, [[prev]]>, IFT, IFF>, env, [prev]>
    : A extends Atom
      ? A extends [`sym`, infer SS]
        ? ReadLet<SS, env> extends Atom & infer U
          ? U
          : [`prim`, ReadLet<SS, env>]
        : A extends ['fn', infer Args, infer Sexpr]
          // note : preventing 2589 error at (A)
          ? ReadLetRecur<A, env> extends infer a ? a : never
          : A
      : A extends LetForm
        ? A extends [`let`, [Sym[], LetVal[]], Sexpr]
          ? A extends [`let`, [infer letsyms, infer letvals], infer LC]
            ? Eval<[`let`, Interleave<letsyms, letvals>, LC], env, [prev]>
            : never
          : A extends [
                `let`,
                [[`sym`, infer LN], infer LV, ...infer LRest],
                infer LC,
              ]
            ? LRest extends [[`sym`, infer LRLN], infer LRLV, ...infer RRest]
              ? Eval<
                  [
                    `let`,
                    [[`sym`, LN], LV],
                    [`let`, [[`sym`, LRLN], LRLV, ...RRest], LC],
                  ],
                  env,
                  [prev]
                >
              : LV extends Prim & [`prim`, infer LP]
                ? // -----------------------
                  // todo : these lvs ugly.
                  // todo : this is picked lp directly,
                  //  unwrapped from [`prim`, ].
                  //  its inconsistency .
                  // -----------------------
                  // ? Eval<LC, Let<LN, LP, env>, [prev]>
                  // -----------------------
                  Eval<LC, Let<LN, LV, env>, [prev]>
                : LV extends Sym & [`sym`, infer LP]
                  ? Eval<LC, Let<LN, ReadLet<LP, env>, env>, [prev]>
                  : LV extends LetForm
                    ? Eval<
                        [`let`, [[`sym`, LN], Eval<LV, env, [prev]>], LC],
                        env,
                        [prev]
                      >
                    : LV extends Fn
                      ? Eval<LC, Let<LN, LV, env>, [prev]>
                    : LV extends Sexpr
                      ? Eval<LC, Let<LN, Eval<LV, env, [[prev]]>, env>, [prev]>
                      : { error: [EvalError7, prev, A] }
          : A extends ['let', [], infer Sexpr]
             ? Eval<Sexpr, env, [prev]>
             : { error: [EvalError8, "this is not proper let-form."]
	      , prev : prev
	      , sexpr : A} // : EvalError9 : EvalError10
        : { error: [EvalError11, prev, A] };

const testletfn0: Eval<['let', [['sym', 'x'], [['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]], ['prim', '0000000000000001']]], [['sym', '*'], ['prim', '0000000000000010'], ['sym', 'x']]]> = ['prim', '0000000000000100']
const testletfn1: Eval<['let', [['sym', 'x'], [['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]], ['prim', '0000000000000001']]], [['sym', '*'], ['prim', '0000000000000010'], ['sym', 'x']]]> = ['prim', '0000000000000100']
const testletfn2: Eval<['let', [['sym', 'x'], ['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]]], [['sym', 'x'], ['prim', '0000000000000010'], ['prim', '0000000000000001']]]> = ['prim', '0000000000000011']
const testletfn3: Eval<['let', [['sym', 'x'], ['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]]], [['sym', 'x'], ['prim', '0000000000000010'], ['prim', '0000000000000001']]]> = ['prim', '0000000000000011']
const testletfn4: Eval<['let', [['sym', 'x'], ['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['sym', 'b'], ['sym', 'a']]]], [['sym', 'x'], ['prim', '0000000000000010'], ['prim', '0000000000000001']]]> = ['prim', '0000000000000011']
// const testletfn5: Compiler.SCompiler<Compiler.SParser<Compiler.SPad<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) y ((fn [a b] (+ a (x 10) b)) 1 8)] (* 2 y))'>>> = ''
const testletfn5: Eval<['let', [['sym', 'x'], ['let', [['sym', 'y'], ['prim', true]], ['sym', 'y']]], ['sym', 'x']]> = ['prim', true]
const testletfn6: Eval<['let', [['sym', 'x'], ['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]]], ['sym', 'x']]> = ['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]]
const testletfn7: Eval<['let', [['sym', 'x'], ['let', [['sym', 'a'], ['prim', '0000000000000010'], ['sym', 'b'], ['prim', '0000000000000010']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]]], ['sym', 'x']]> = ['prim', '0000000000000100']
const testletfn8: Eval<['let', [['sym', 'x'], ['let', [['sym', 'a'], ['prim', '0000000000000011'], ['sym', 'b'], ['prim', '0000000000000010']], ['fn', [['sym', 'c']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]]]], ['sym', 'x']]> = ['fn', [['sym', 'c']], [['sym', '+'], ['prim', '0000000000000011'], ['prim', '0000000000000010']]]



// test get
const evallisp_get_0: IsKeyMapSexpr<[['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]]> = true
const evallisp_get_1: Eval<[['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]]> = ['prim', '0']
const test_get_0: LispGet<[['map', [['key', ':a'], ['prim', '0']]], ['key', ':a']]> = ['prim', '0']
const test_get_1: LispGet<[['map', [['key', ':'], ['prim', '0']]], ['key', ':a']]> = []
const evallisp_get_2: IsKeyMapSexpr<[['map', [['key', ':a'], ['prim', '0']]],['key', ':a']]> = true
const evallisp_get_3: Eval<[['map', [['key', ':a'], ['prim', '0']]], ['key', ':a']]> = ['prim', '0']

// test case
type Tdsds = [[`sym`, `AppendP`], [`prim`, `'test'`]];
type Tdddd = [[`sym`, `AppendP`], [`sym`, `str`]];

// test raw
const evalTest: Eval<Tdsds, [[]]> = [`prim`, "'+test'"];
const evalTest2: Eval<Tdddd, [[]]> = {
  error: ["AppendError", ["prim", "NotMatch"]],
};
const evalTest3: Eval<Tdddd, [[MakeVar<`str`, `'strval'`>]]> = [
  `prim`,
  "'+strval'",
];
const evalTest4: Eval<
  Tdddd,
  [[MakeVar<"str", "'strval'">], [MakeVar<"sstr", "'notstrval'">]]
> = [`prim`, "'+strval'"];
const evalTest5: Eval<
  Tdddd,
  [[MakeVar<"sstr", "'notstrval'">], [MakeVar<"str", "'strval'">]]
> = [`prim`, "'+strval'"];

// test fn
const evalfntest: Eval<
  [[`fn`, [[`sym`, `str`]], Tdddd], [`prim`, `'test'`]],
  [[MakeVar<"str", "'strval'">]]
> = [`prim`, "'+test'"];
const evalfntest2: Eval<
  [[`fn`, [[`sym`, `str`]], Tdddd], [`prim`, `'test'`]],
  [[MakeVar<"aaa", "'aaa'">], [MakeVar<"str", "'strval'">]]
> = [`prim`, "'+test'"];

// test fn sym
const evalfnsymrawtest: Eval<
  [
    [`fn`, [[`sym`, `a`]], [[`sym`, `AppendP`], [`sym`, `a`]]],
    [`prim`, `'test'`],
  ]
> = [`prim`, `'+test'`];
const evalfnsymtest: Eval<
  [[`sym`, `f`], [`prim`, `'test'`]],
  [[MakeVar<"f", [`fn`, [[`sym`, `a`]], [[`sym`, `AppendP`], [`sym`, `a`]]]>]]
> = [`prim`, `'+test'`];

// test atomic
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

// test let
// const evallettest: Eval<[`let`, [[`sym`, `t`], `'test'`], [`sym`, `t`]]> = [`prim`, "'test'"] // deprecated.
const evallettest: Eval<[`let`, [[`sym`, `t`], `'test'`], [`sym`, `t`]]> = {
  error: [
    "EvalError7/ 2nd in let form must be Atom. No wrapped value is deprecated.",
    0,
    ["let", [["sym", "t"], "'test'"], ["sym", "t"]],
  ],
};
const evalletwprimtest: Eval<
  [`let`, [[`sym`, `t`], [`prim`, `'test'`]], [`sym`, `t`]],
  []
> = [`prim`, "'test'"];

// recursive test[fn]
type AppendPWstr = [[`sym`, `AppendP`], [`sym`, `str`]];
type AppendPWa = [[`sym`, `AppendP`], [`sym`, `a`]];
type InnerFnTest = [`fn`, [[`sym`, `str`]], AppendPWstr];
type OuterFnTest = [`fn`, [[`sym`, `a`]], [InnerFnTest, [`sym`, `a`]]];
const evalrecfntest1: Eval<[OuterFnTest, [`prim`, `'test'`]], []> = [
  `prim`,
  `'+test'`,
];

// recursive test[let]
type InnerLetTest = [
  `let`,
  [[`sym`, `str`], [`prim`, `'test'`]],
  [[`sym`, `AppendP`], [`sym`, `str`]],
];
type OuterLetTest = [
  `let`,
  [[`sym`, `aaa`], InnerLetTest],
  [[`sym`, `AppendP`], [`sym`, `aaa`]],
];
type RecLetTest = [
  `let`,
  [[`sym`, `aaa`], InnerLetTest],
  [
    `let`,
    [[`sym`, `bbb`], [`sym`, `aaa`]],
    [[`sym`, `AppendP`], [`sym`, `bbb`]],
  ],
];
const evalreclettest0: Eval<InnerLetTest> = [`prim`, `'+test'`];
const evalreclettest1: Eval<OuterLetTest> = [`prim`, `'++test'`];
const evalreclettest2: Eval<RecLetTest> = [`prim`, `'++test'`];

// recursive test[fn in let]
type FlInnerTest = [
  `let`,
  [[`sym`, `str`], [`fn`, [[`sym`, `a`]], [[`sym`, `AppendP`], [`sym`, `a`]]]],
  [[`sym`, `str`], [`prim`, `'test'`]],
];
const evalfltest0: Eval<FlInnerTest> = [`prim`, `'+test'`];

// // recursive test[let in fn]
// type LfInnerTest = [
//   `fn`,
//   [[`sym`, `fnarg`]],
//   [
//     `let`,
//     [
//       [`sym`, `str`],
//       [`fn`, [[`sym`, `a`]], [[`sym`, `AppendP`], [`sym`, `a`]]],
//     ],
//     [[`sym`, `str`], [`sym`, `fnarg`]],
//   ],
// ];
// // ----------------------------
// // todo : gross error msg.
// // src/index.ts:218:7 - error TS2322: Type 'string[]' is not assignable to type '"AppendError"'.
// //
// // 218 const evallftest0: Eval<[lfInnerTest, [`prim`, `test''`]]> = [`prim`, `'+test'`]
// //
// // const evallftesterr: Eval<[lfInnerTest, [`prim`, `test''`]]> = [`prim`, `'+test'`]
// // ----------------------------
// const evallftest0: Eval<[LfInnerTest, [`prim`, `'test'`]]> = [
//   `prim`,
//   `'+test'`,
// ];


// test interleaved let form
const testiletform: Eval<
  [
    `let`,
    [[[`sym`, `a`], [`sym`, `b`]], [[`prim`, `'1'`], [`prim`, `'2'`]]],
    [[`sym`, `str`], [`sym`, `a`], [`sym`, `b`]],
  ]
> = [`prim`, `'12'`];

// test if
type IfTruePrimTest = [`if`, [`prim`, true], [`prim`, true], [`prim`, false]];
type IfFalsePrimTest = [`if`, [`prim`, false], [`prim`, true], [`prim`, false]];
const evaliftest2: Eval<IfTruePrimTest> = [`prim`, true];
const evaliftest3: Eval<IfFalsePrimTest> = [`prim`, false];
type IfRecTrueTest = [`if`, IfTruePrimTest, [`prim`, true], [`prim`, false]];
const evalifrectest0: Eval<IfRecTrueTest> = [`prim`, true];
type IfRecFalseTest = [`if`, IfFalsePrimTest, [`prim`, true], [`prim`, false]];
const evalifrectest1: Eval<IfRecFalseTest> = [`prim`, false];
type IfRetFnPattern = [
  `if`,
  IfFalsePrimTest,
  [`fn`, [[`sym`, `ifa`]], [[`sym`, `AppendP`], [`sym`, `ifa`]]],
  [`fn`, [[`sym`, `ifa`]], [[`sym`, `AppendP`], [`sym`, `ifa`]]],
];
type IfRetFnSexpr = [IfRetFnPattern, [`prim`, `'test'`]];
const evalifretfntest: Eval<IfRetFnSexpr> = [`prim`, `'+test'`];

// test str
const evalstrtest: Eval<[[`sym`, `str`], [`prim`, `head/`], [`prim`, `tail`]]> =
  [`prim`, `'head/tail'`];

// test let >1
type Letmoretest = [
  `let`,
  [[`sym`, `a`], [`prim`, `text-a`], [`sym`, `b`], [`prim`, `/text-b`]],
  [[`sym`, `str`], [`sym`, `a`], [`sym`, `b`]],
];
const evalletmoretest1: Eval<Letmoretest> = [`prim`, `'text-a/text-b'`];
const aaaaaaaaa: LetForm = [
  `let`,
  [
    [`sym`, `a`],
    [`prim`, `text-a`],
    [`sym`, `b`],
    [`prim`, `text-b`],
  ],
  [
    [`sym`, `str`],
    [`sym`, `a`],
    [`sym`, `b`],
  ],
];

// test prim error pattern.
const evalprimerrortest: Eval<[`prim`, 0]> = [`prim`, 0];

// test lispeq
const evallispeqtest0: Eval<[[`sym`, `eq`], [`prim`, 0], [`prim`, 0]]> = [
  `prim`,
  true,
];
const evallispeqtest1: Eval<[[`sym`, `eq`], [`prim`, 1], [`prim`, 0]]> = [
  `prim`,
  false,
];
const evallispeqtest2: Eval<
  [[`sym`, `eq`], [`prim`, 0], [`prim`, 0], [`prim`, 0]]
> = [`prim`, true];





// ------------------------------------------
// the above is in the case of not recursive sexpr.
// -----------------------------------------

type Error1 = "RecEval1";
type Error2 = "RecEval2";
type Error3 = "RecEval3";
type Error4 = "RecEval4";
type Error5 = "RecEval5";

type RecEval<A, env> = env extends EnvLifo
  ? A extends [infer OPC, infer OPR]
    ? OPC extends Sexpr
      ? OPR extends Sexpr
        ? Eval<
            [
              RecEval<OPC, env> extends infer A ? A : never,
              RecEval<OPR, env> extends infer B ? B : never,
            ],
            env
          >
        : OPR extends Atom
          ? Eval<[RecEval<OPC, env> extends infer A ? A : never, OPR], env>
          : Error1
      : OPC extends Atom
        ? OPR extends Sexpr
          ? Eval<[OPC, RecEval<OPR, env> extends infer A ? A : never], env>
          : OPR extends Atom
            ? Eval<[OPC, OPR], env>
            : Error2
        : Error3
    : Error4
  : Error5;

//test (no env)
const rbiTest: RecEval<[[`sym`, `AppendP`], [`prim`, `'test'`]], []> = [
  `prim`,
  "'+test'",
];
const rbiTest2: RecEval<
  [[`sym`, `AppendP`], [[`sym`, `AppendP`], [`prim`, `'test'`]]],
  []
> = [`prim`, "'++test'"];
const rbiTest3: RecEval<
  [
    [`sym`, `AppendP`],
    [[`sym`, `AppendP`], [[`sym`, `AppendP`], [`prim`, `'test'`]]],
  ],
  []
> = [`prim`, "'+++test'"];
// test (with env)
const rbiTest4: RecEval<
  [[`sym`, `AppendP`], [`sym`, `testsym`]],
  [[MakeVar<"testsym", `'test'`>]]
> = [`prim`, "'+test'"];
const rbiTest5: RecEval<
  [[`sym`, `AppendP`], [`sym`, `testsym`]],
  [
    [MakeVar<"testsym", `'test'`>],
    [MakeVar<"t", "'t'">],
    [MakeVar<"tttt", "'tttt'">],
  ]
> = [`prim`, "'+test'"];

// --------------------
// def / defn a scribble.
// biome-ignore lint/suspicious/noExplicitAny:
const defined: [`sym`, `test`] = null as any;
const readdef: typeof defined = [`sym`, `test`]; // null as any
// const readdef2: typeof defined = [`sym`, `tet`] // null as any

// ----------------------------
// -- Main
// ----------------------------

type Lisp<S extends string> = Eval<Compiler.SCompiler<Compiler.SParser<Compiler.SPad<S>>>>

export default Lisp
