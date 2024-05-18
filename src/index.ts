type Each = LetForm | IfForm | Atom;
type Atom = Sym | Prim | Fn | Vector | Nil;
// This isn't usually way to define a sexpr, not including atomic something.
// That role leaves to EACH.
type Sexpr = Array<Each | Sexpr>;
type Nil = []
const Nil: Nil = []

type Sym = [`sym`, string];
type Prim = [`prim`, string | boolean | number]; // todo : This boolean is appended IFForm, using boolean directly in current.
type Args = Sym[];
type Fn = [`fn`, Args, Each | Array<Each>];

type Var = {
  name: string;
  value: string | Atom;
};

type Env = Var[];

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

type Let<N, V, EnvLifo = Env[]> = EnvLifo extends Env[]
  ? [...EnvLifo, [MakeVar<N, V>]]
  : never;

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

type Reading<AS, EnvLifo = [[]], prev = 0, R = []> = R extends Array<Atom> // todo : ugly
  ? AS extends Atom[] & [infer H, ...infer T]
    ? Reading<T, EnvLifo, prev, [...R, ReadAtom<H, EnvLifo, prev>]>
    : R
  : never;
// test reading
const readingtest: Reading<
  [[`sym`, `a`], [`sym`, `b`], [`prim`, `c-str`]],
  [[MakeVar<"a", [`prim`, "a-str"]>, MakeVar<"b", [`prim`, "b-str"]>]]
> = [
  ["prim", "a-str"],
  ["prim", "b-str"],
  ["prim", "c-str"],
];

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
  : {error: [AppendError, S]};

const appendTest: AppendP<[`prim`, "'test'"]> = [`prim`, "'+test'"];

// note: if `R extends string` doesn't exist, R cannot be passed into `${R}` because ts can get the type of R.
type Str<S, R = ""> = R extends string
  ? S extends [[`prim`, `${infer HS}`], ...infer T]
    ? Str<T, `${R}${HS}`>
    : [`prim`, R]
  : never;
// test str
const strtest1: Str<[[`prim`, `test`], [`prim`, `+`], [`prim`, `tail`]]> = [
  `prim`,
  `test+tail`,
];
const strtest2: Str<[[`prim`, `test`]]> = [`prim`, `test`];

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

// todo : naming
// let itself isn't value and returning value, unlike fn, so maybe ok as it is.
// todo
// integrated let form to [[`sym`, string], EACH | Array<EACH>] in future.
type LetVal = string | Each | Each[];
type LetArg = [Sym, LetVal];
// todo : too ugly.
type LetForm = [`let`, (Sym | LetVal)[], Each | Each[]];
// test let
const larttest: LetArg = [[`sym`, `t`], `test`];

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
type HashMap = [`HashMap`, { [others: string]: Atom }];
type GetMapError0 = "GetMapError0";
type GetMapError1 = "GetMapError1";
type GetMapError2 = "GetMapError2";
type GetMap<N, V> = N extends string
  ? V extends HashMap & [`HashMap`, infer W]
    ? W extends { [others: string]: Atom }
      ? W[N]
      : GetMapError0
    : GetMapError1
  : GetMapError2;

const testgetmap: GetMap<
  "a",
  [`HashMap`, { a: [`prim`, `val/a`]; b: [`prim`, `val/b`] }]
> = [`prim`, `val/a`];

type Get<K, V> = V extends Vector ? GetVec<K, V> : GetMap<K, V>;
const testgetvec1: Get<
  3,
  [`vec`, [`prim`, 0], [`prim`, 1], [`prim`, 2], [`prim`, 3], [`prim`, 4]]
> = [`prim`, 3];
const testgetmap1: Get<
  "a",
  [`HashMap`, { a: [`prim`, `val/a`]; b: [`prim`, `val/b`] }]
> = [`prim`, `val/a`];

// fns of seq
type FirstError = "FirstError";
type RestError = "RestError";
type ConjError = "ConjError";
type ConcatError = "ConcatError";
type First<V> = V extends Vector & [`vec`, infer H, ...infer T]
  ? H
  : FirstError;
type Rest<V> = V extends Vector & [`vec`, infer H, ...infer T]
  ? T[0] extends Atom
    ? [`vec`, ...T]
    : [`vec`]
  : RestError;
type Conj<V, E> = E extends Atom
  ? V extends Vector
    ? [...V, E]
    : ConjError : ConjError
type Concat<V, W> = V extends Vector
  ? W extends Vector & [`vec`, ...infer WW]
    ? [...V, ...WW]
    : ConcatError
  : ConcatError;
// get, assoc, update
// type Get<S, K> = S extends Vector & [`vec`, infer V] ? K extends number ? V[K] : never : S extends HashMap & [`HashMap`, infer V] ? K extends string ? V[K] : never : never

type testvec = [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1]]
const testfirst: First<testvec> = [`prim`, true]
const testrest: Rest<testvec> = [`vec`, [`prim`, 0], [`prim`, 1]]
const testrest1: Rest<Rest<testvec>> = [`vec`, [`prim`, 1]]
const testrest2: Rest<Rest<Rest<testvec>>> = [`vec`]
const testconj: Conj<testvec, [`prim`, false]> =  [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1], [`prim`, false]]
const testconj1: Conj<[`vec`], [`prim`, false]> = [`vec`, [`prim`, false]]
const testconcat: Concat<testvec, testvec> =  [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1],  [`prim`, true], [`prim`, 0], [`prim`, 1], ]



// map, filter, remove, every, some
type FMapError = "MapError";
type FilterError = "FilterError";
type RemoveError = "RemoveError";
type EveryError = "EveryError";
type SomeError = "SomeError";
type FMappable<F, V> = F extends Fn | Sym ? (V extends Vector ? V : never) : never;
type FMap<F, V, Env = [[]], prev = [0], init = true> = FMappable<F, V> extends Vector & [
  `vec`,
  infer H,
  ...infer T,
]
   ? Eval<[F, H, ...T]> : never
// test fmaps
type testf = Sym & [`sym`, `AppendP`]
// if not directly input those sexpr, through args, this fmap eval returns any, because of ...infer T (in FMap) would be expanded unknown.
const testargv: testargv = [`vec`, [`prim`, `1`], [`prim`, `2`]]
const testfmappable: FMappable<[`sym`, `AppendP`], typeof testargv> = testargv
const testfmap: FMap<[`sym`, `AppendP`], typeof testargv> = 1

const alala: Eval<[[`sym`, `AppendP`], [`prim`, `'1'`]]> = 1


// -------------------------
// bit ops
type BitOr<B, C> = B extends `${infer BH}${infer BR}`
  ? C extends `${infer CH}${infer CR}`
    ? CH extends `1`
      ? `1${BitOr<BR, CR>}`
      : BH extends `1`
        ? `1${BitOr<BR, CR>}`
        : `0${BitOr<BR, CR>}`
    : ``
  : ``;
const bitor1: BitOr<`1`, `1`> = `1`;
const bitor2: BitOr<`1`, `0`> = `1`;
const bitor3: BitOr<`0`, `1`> = `1`;
const bitor4: BitOr<`0`, `0`> = `0`;
const bitor5: BitOr<`010`, `000`> = `010`;
const bitor6: BitOr<`111`, `111`> = `111`;
const bitor7: BitOr<`110`, `110`> = `110`;
const bitor8: BitOr<`000`, `000`> = `000`;

type BitAnd<B, C> = B extends `${infer BH}${infer BR}`
  ? C extends `${infer CH}${infer CR}`
    ? CH extends `1`
      ? BH extends `1`
        ? `1${BitAnd<BR, CR>}`
        : `0${BitAnd<BR, CR>}`
      : `0${BitAnd<BR, CR>}`
    : ``
  : ``;
const bitand1: BitAnd<`1`, `1`> = `1`;
const bitand2: BitAnd<`1`, `0`> = `0`;
const bitand3: BitAnd<`0`, `1`> = `0`;
const bitand4: BitAnd<`0`, `0`> = `0`;
const bitand5: BitAnd<`010`, `000`> = `000`;
const bitand6: BitAnd<`111`, `111`> = `111`;
const bitand7: BitAnd<`110`, `110`> = `110`;
const bitand8: BitAnd<`000`, `000`> = `000`;

type BitXor<B, C> = B extends `${infer BH}${infer BR}`
  ? C extends `${infer CH}${infer CR}`
    ? CH extends BH
      ? `0${BitXor<BR, CR>}`
      : `1${BitXor<BR, CR>}`
    : ``
  : ``;
const bitxor1: BitXor<`1`, `1`> = `0`;
const bitxor2: BitXor<`1`, `0`> = `1`;
const bitxor3: BitXor<`0`, `1`> = `1`;
const bitxor4: BitXor<`0`, `0`> = `0`;
const bitxor5: BitXor<`010`, `000`> = `010`;
const bitxor6: BitXor<`111`, `111`> = `000`;
const bitxor7: BitXor<`110`, `110`> = `000`;
const bitxor8: BitXor<`000`, `000`> = `000`;
const bitxor9: BitXor<`101`, `001`> = `100`;

// -------------------
// memo
// type SSS<B> = B extends `${infer U}` ? U[0] : never
// const sss: SSS<`abcde`> = 1
// // error TS2322: Type 'number' is not assignable to type 'string'.
// -------------------

// // --------------------------------
// // memo
// type tTTT<T> = T extends [infer A, ...infer B] ? B[1] : never
// const tttt: tTTT<[[1,2],[2,3],[3,4]]> = [3,4]
// type tTTTT<T> = T extends [...infer A, ...infer B] ? B[1] : never
// const ttttt: tTTTT<[[1,2],[2,3],[3,4]]> = [3,4]
// type tTTTB0<T> = T extends [infer A, ...infer B] ? B[0] : never
// const ttttB0: tTTTB0<[[1,2],[2,3],[3,4]]> = [2,3]
// // ------------------
// // type tTTTA<T> = T extends [infer A, ...infer B] ? A[1] : never
// // const ttttA: tTTTA<[[1,2],[2,3],[3,4]]> = [3,4]
// // src/index.ts:164:51 - error TS2536: Type '1' cannot be used to index type 'A'.
// //
// // 164 type tTTTA<T> = T extends [infer A, ...infer B] ? A[1] : never
// // -------------------
// type tTTTTA<T> = T extends [...infer A, ...infer B] ? A[1] : never
// const tttttA: tTTTTA<[[1,2],[2,3],[3,4]]> = [3,4]
// type tTTTTAA<T> = T extends [...infer A, ...infer B] ? A[0] : never
// const tttttAA: tTTTTAA<[[1,2],[2,3],[3,4]]> = [3,4]
// type tTTTT__1<T> = T extends [...infer A, ...infer B] ? A[1] : never
// const ttttt__1: tTTTTA<[[1,2],[2,3],[3,4]]> = [3,4]
// type tTTTT__2<T> = T extends [...infer A, ...infer B] ? A[0] : never
// const ttttt__2: tTTTTAA<[[1,2],[2,3],[3,4]]> = [3,4]
// // --------------------------

// todo : ugly
type Eval<A, env = [[]], prev = 0> = A extends Sexpr
  ? A extends [infer OPC, ...infer OPR]
    ? env extends EnvLifo
      ? OPC extends Fn & [`fn`, [[`sym`, infer S]], infer D]
        ? OPR[0] extends Sym & [`sym`, infer VV]
          ? Eval<D, Let<S, ReadLet<VV, env>, env>, [prev]>
          : OPR[0] extends Prim & [`prim`, infer VVV]
            ? Eval<D, Let<S, VVV, env>, [prev]>
            : EvalError5
        : OPC extends IfForm & [`if`, infer IFCond, infer IFT, infer IFF]
          ? Eval<
              [If<Eval<IFCond, env, [[prev]]>, IFT, IFF>, OPR[0]],
              env,
              [prev]
            >
          : OPC extends Sym & [`sym`, infer U]
            ? // care of double-booking.
              ReadLet<U, env> extends NotMatch // `AppendP` | `str`
              ? U extends `AppendP`
                ? AppendP<ReadAtom<Eval<OPR[0], env, [[prev]]>, env, [prev]>>
                : U extends `str`
                  ? // Reading<OPR, env, [[prev]]>
                    Str<Reading<OPR, env, [[prev]]>>
                  : Eval<[ReadLet<U, env>, OPR[0]], env, [prev]>
              : // this is fn case.
                ReadLet<U, env> extends Fn & infer UU
                ? Eval<[UU, OPR[0]], env, [prev]>
                : EvalError3
            : EvalError4
      : EvalError6
    : EvalError2
  : A extends IfForm & [`if`, infer IFCond, infer IFT, infer IFF]
    ? Eval<If<Eval<IFCond, env, [[prev]]>, IFT, IFF>, env, [prev]>
    : A extends Atom
      ? A extends [`sym`, infer SS]
        ? ReadLet<SS, env> extends Atom & infer U
          ? U
          : [`prim`, ReadLet<SS, env>]
        : A
      : A extends LetForm
        ? A extends [
            `let`,
            [[`sym`, infer LN], infer LV, ...infer LRest],
            infer LC,
          ]
          ? LRest extends [[`sym`, infer LRLN], infer LRLV]
            ? // [`let`, [[`sym`, LN], LV], [`let`, [[`sym`, LRLN], LRLV], LC]]
              Eval<
                [`let`, [[`sym`, LN], LV], [`let`, [[`sym`, LRLN], LRLV], LC]],
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
                    : { error: [EvalError7, prev, A] }
          : { error: [EvalError8, prev, A] } // : EvalError9 : EvalError10
        : { error: [EvalError11, prev, A] };

// test case
type Tdsds = [[`sym`, `AppendP`], [`prim`, `'test'`]];
type Tdddd = [[`sym`, `AppendP`], [`sym`, `str`]];

// test raw
const evalTest: Eval<Tdsds, [[]]> = [`prim`, "'+test'"];
const evalTest2: Eval<Tdddd, [[]]> = { error: ["AppendError", ["prim", "NotMatch"]]};
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

// recursive test[let in fn]
type LfInnerTest = [
  `fn`,
  [[`sym`, `fnarg`]],
  [
    `let`,
    [
      [`sym`, `str`],
      [`fn`, [[`sym`, `a`]], [[`sym`, `AppendP`], [`sym`, `a`]]],
    ],
    [[`sym`, `str`], [`sym`, `fnarg`]],
  ],
];
// ----------------------------
// todo : gross error msg.
// src/index.ts:218:7 - error TS2322: Type 'string[]' is not assignable to type '"AppendError"'.
//
// 218 const evallftest0: Eval<[lfInnerTest, [`prim`, `test''`]]> = [`prim`, `'+test'`]
//
// const evallftesterr: Eval<[lfInnerTest, [`prim`, `test''`]]> = [`prim`, `'+test'`]
// ----------------------------
const evallftest0: Eval<[LfInnerTest, [`prim`, `'test'`]]> = [
  `prim`,
  `'+test'`,
];

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
  [`prim`, `head/tail`];

// test let >1
type Letmoretest = [
  `let`,
  [[`sym`, `a`], [`prim`, `text-a`], [`sym`, `b`], [`prim`, `/text-b`]],
  [[`sym`, `str`], [`sym`, `a`], [`sym`, `b`]],
];
const evalletmoretest1: Eval<Letmoretest> = [`prim`, `text-a/text-b`];
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
