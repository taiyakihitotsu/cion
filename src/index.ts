type EACH = LetForm | ATOM
type ATOM  = Sym | Prim | Fn
type SEXPR = Array<EACH | SEXPR>

type Sym  = [`sym`, string]
type Prim = [`prim`, string]
type Args = Sym[]
type Fn   = [`fn`, Args, EACH | Array<EACH>]

type Var = {
  name: string
  , value: string | ATOM
}

type Env = Var[]

type NotMatch = "NotMatch"
const NotMatch = "NotMatch"

type MakeVar<N, V> = {name: N, value: V}

type GetVar<T, E> = 
  E extends Env
  ? E extends [infer U, ...infer R]
    ? U extends Var
      ? U["name"] extends T
        ? U["value"]
        : GetVar<T, R> : NotMatch : NotMatch : NotMatch

// test
const getVarTest: GetVar<"s", [MakeVar<"ss", "stringer">, MakeVar<"s", "string">]> = "string"
const getVarTest2: GetVar<"ss", [MakeVar<"ss", "stringer">, MakeVar<"s", "string">]> = "stringer"
const EvalTest3: GetVar<"sss", [MakeVar<"ss", "stringer">, MakeVar<"s", "string">]> = NotMatch

//------------------------------------------------

// Let
// Env in Let must be a list of LIFO.
//
// Note:
// This "Lifo" has no mean but just a naming.
//
// Sym in Sym itself is loadable but ReadLet doesn't do that
// because this case is broken in a macro context.
type EnvLifo = Env[]

type Let<N,V,EnvLifo = Env[]> = EnvLifo extends Env[] ? [...EnvLifo, [MakeVar<N,V>]] : never

type ReadLet<N, EnvLifo> = EnvLifo extends [...infer HS, infer L] ? L extends Env ? GetVar<N, L> extends NotMatch ? ReadLet<N, HS> : GetVar<N, L> : NotMatch : NotMatch

// test
type LetEnvLifo = [[MakeVar<"ss", "stringer">, MakeVar<"s", "string">, MakeVar<"cc", [`prim`, "p/cc"]>]]
const letTest: Let<"sss", "str", LetEnvLifo> = [[{ name: "ss", value: "stringer" }, { name: "s", value: "string" }, {name: "cc", value: [`prim`, `p/cc`]}], [{ name: "sss", value: "str" }]]

const readLetTest: ReadLet<"s", Let<"sss", "str", LetEnvLifo>> = "string"
const readLetTest2:  ReadLet<"sss", Let<"sss", "str", LetEnvLifo>> = "str"
const readLetTest3:  ReadLet<"ssss", Let<"sss", "str", LetEnvLifo>> = NotMatch
// test / primitive - case
const readLetTest4: ReadLet<"sss", Let<"sss", [`prim`, `p/sss`], LetEnvLifo>> = [`prim`, `p/sss`]
const readLetTest5:  ReadLet<"cc", Let<"sss", "str", LetEnvLifo>> = [`prim`, `p/cc`]

//-----------------------------------------

// Def
// 
// Note: Ignore duplicate def (in current)
//   to fix it, making error handling type.

type Def<N,V,EnvLifo> = EnvLifo extends [infer U, ...infer R] ? U extends Env ? [[MakeVar<N,V>, ...U], ...R] : never :never

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

type AppendError = "AppendError"
type AppendP<S> = S extends `'${infer U}'` ? [`prim`, `'+${U}'`] : AppendError

const appendTest: AppendP<"'test'"> = [`prim`, "'+test'"]

type EvalError1 = "EvalError1"
type EvalError2 = "EvalError2"
type EvalError3 = "EvalError3"
type EvalError4 = "EvalError4"
type EvalError5 = "EvalError5"
type EvalError6 = "EvalError6" 
type EvalError7 = "EvalError7/ 2nd in let form must be Atom. No wrapped value is deprecated."
type EvalError8 = "EvalError8"
type EvalError9 = "EvalError9"
type EvalError10 = "EvalError10"
type EvalError11 = "EvalError11"
type EvalError12 = "EvalError12"

// todo : naming
// let itself isn't value and returning value, unlike fn, so maybe ok as it is.
// todo
// integrated let form to [[`sym`, string], EACH | Array<EACH>] in future.
type LetArg = [[`sym`, string], string | EACH | Array<EACH> ]
type LetForm = [`let`, LetArg, EACH | Array<EACH>]
// test let
const larttest: LetArg = [[`sym`, `t`], `test`]


// test fn
const dectest: Fn = [`fn`, [[`sym`, `a`]], [[`sym`, `a`], [`sym`, `b`]]]

// todo : ugly
type Eval<A, env = [[]], prev = 0> =
  A extends SEXPR
  ? A extends [infer OPC, infer OPR]
    ? env extends EnvLifo
	? OPC extends Fn
	  ? OPC extends Fn & [`fn`, [[`sym`, infer S]], infer D]
	    ? OPR extends Sym & [`sym`, infer VV]
	      ? Eval<D, Let<S,ReadLet<VV, env>,env>, [prev]>
	    : OPR extends Prim & [`prim`, infer VVV]
	      ? Eval<D, Let<S,VVV,env>, [prev]>
	    : EvalError5
            : EvalError7
	: OPC extends Sym & [`sym`, infer U]
          // todo : this shouldn't be build-in.
          //   if making def, or global env,
          //   this part will be deleted.
	  ? U extends `AppendP`
	    ? OPR extends [`sym`, infer V]
	      // todo : in current,
	      //   if a sym isn't matched with an env,
	      //   this returns appendP error, not env error (I hope to return .)
	      ? AppendP<ReadLet<V, env>>
	    : OPR extends [`prim`, infer W]
	      ? AppendP<W>
              : EvalError1
          // this is fn case.
          : ReadLet<U,env> extends Fn & infer UU
            ? Eval<[UU, OPR], env, [prev]>
            : EvalError3
        : EvalError4 : EvalError6 : EvalError2
  : A extends ATOM
    ? A extends [`sym`, infer SS]
      ? ReadLet<SS, env> extends ATOM & infer U
      ? U
      : [`prim`, ReadLet<SS, env>]
      : A
  : A extends LetForm
    ? A extends [`let`, [[`sym`, infer LN], infer LV], infer LC]
      ? LV extends Prim & [`prim`, infer LP]
      // todo : these lvs ugly.
      ? Eval<LC, Let<LN, LP, env>, [prev]>
      : LV extends Sym & [`sym`, infer LP]
      ? Eval<LC, Let<LN, ReadLet<LP, env>, env>, [prev]>
      : LV extends LetForm 
      ? Eval<[`let`, [[`sym`, LN], Eval<LV, env, [prev]>], LC], env, [prev]>
      : LV extends Fn
      ? Eval<LC, Let<LN, LV, env>, [prev]>
      : {error: [EvalError7, prev, A]} : EvalError8 // : EvalError9 : EvalError10
    : {error: [EvalError11, prev, A]}


// test case
type TDSDS = [[`sym`, `AppendP`], [`prim`, `'test'`]]
type TDDDD = [[`sym`, `AppendP`], [`sym`, `str`]]
// test raw
const evalTest: Eval<TDSDS, [[]]> = [`prim`, "'+test'"]
const evalTest2: Eval<TDDDD, [[]]> = "AppendError"
const evalTest3: Eval<TDDDD, [[MakeVar<`str`,`'strval'`>]]> = [`prim`,"'+strval'"]
const evalTest4:  Eval<TDDDD, [[MakeVar<"str", "'strval'">], [MakeVar<"sstr", "'notstrval'">]]> = [`prim`, "'+strval'"]
const evalTest5:  Eval<TDDDD, [[MakeVar<"sstr", "'notstrval'">], [MakeVar<"str", "'strval'">]]> = [`prim`, "'+strval'"]
// test fn
const evalfntest: Eval<[[`fn`, [[`sym`, `str`]], TDDDD], [`prim`, `'test'`]], [[MakeVar<"str", "'strval'">]]> = [`prim`, "'+test'"]
const evalfntest2: Eval<[[`fn`, [[`sym`, `str`]], TDDDD], [`prim`, `'test'`]], [[MakeVar<"aaa", "'aaa'">], [MakeVar<"str", "'strval'">]]> = [`prim`, "'+test'"]
// test fn sym
const evalfnsymrawtest: Eval<[[`fn`, [[`sym`, `a`]], [[`sym`, `AppendP`], [`sym`, `a`]]], [`prim`, `'test'`]]> = [`prim`, `'+test'`]
const evalfnsymtest: Eval<[[`sym`, `f`], [`prim`, `'test'`]], [[MakeVar<"f", [`fn`, [[`sym`, `a`]], [[`sym`, `AppendP`], [`sym`, `a`]]]>]]> = [`prim`, `'+test'`]
// test atomic
const evalatomtest: Eval<[`prim`, `'test'`]> = [`prim`, `'test'`]
const evalatomtest2: Eval<[`sym`, `test`],[[MakeVar<`test`, `'testval'`>]]> = [`prim`, `'testval'`]
const evalatomtest3: Eval<[`sym`, `test`], [[MakeVar<`test`, [`prim`, `'prim/test'`]>]]> = [`prim`, `'prim/test'`]
const evalatomtest4: Eval<[`sym`, `test`], [[MakeVar<`test`, [`fn`, [[`sym`, `a`]], [`sym`, `a`]]>]]> = [`fn`, [[`sym`, `a`]], [`sym`, `a`]]
// test let
// const evallettest: Eval<[`let`, [[`sym`, `t`], `'test'`], [`sym`, `t`]]> = [`prim`, "'test'"] // deprecated.
const evallettest: Eval<[`let`, [[`sym`, `t`], `'test'`], [`sym`, `t`]]> = { error: ["EvalError7/ 2nd in let form must be Atom. No wrapped value is deprecated.", 0, ["let", [["sym", "t"], "'test'"], ["sym", "t"]]]}
const evalletwprimtest: Eval<[`let`, [[`sym`, `t`], [`prim`, `'test'`]], [`sym`, `t`]], []> = [`prim`, "'test'"]


// recursive test[fn]
type AppendPWstr = [[`sym`, `AppendP`], [`sym`, `str`]]
type AppendPWa   = [[`sym`, `AppendP`], [`sym`, `a`]]
type InnerFnTest = [`fn`, [[`sym`, `str`]], AppendPWstr]
type OuterFnTest = [`fn`, [[`sym`, `a`]], [InnerFnTest, [`sym`, `a`]]]
const evalrecfntest1: Eval<[OuterFnTest, [`prim`, `'test'`]], []> = [`prim`, `'+test'`]

// recursive test[let]
type InnerLetTest = [`let`, [[`sym`, `str`], [`prim`, `'test'`]], [[`sym`, `AppendP`], [`sym`, `str`]]]
type OuterLetTest = [`let`, [[`sym`, `aaa`], InnerLetTest], [[`sym`, `AppendP`], [`sym`, `aaa`]]]
type RecLetTest = [`let`,[[`sym`, `aaa`], InnerLetTest], [`let`, [[`sym`, `bbb`], [`sym`, `aaa`]], [[`sym`, `AppendP`], [`sym`, `bbb`]]]]
const evalreclettest0: Eval<InnerLetTest> = [`prim`, `'+test'`]
const evalreclettest1: Eval<OuterLetTest> = [`prim`, `'++test'`]
const evalreclettest2: Eval<RecLetTest> = [`prim`, `'++test'`]

// recursive test[fn in let]
type flInnerTest = [`let`, [[`sym`, `str`], [`fn`, [[`sym`, `a`]], [[`sym`, `AppendP`], [`sym`, `a`]]]], [[`sym`, `str`], [`prim`, `'test'`]]]
const evalfltest0: Eval<flInnerTest> = [`prim`, `'+test'`]

// recursive test[let in fn]
type lfInnerTest =  [`fn`, [[`sym`, `fnarg`]], [`let`, [[`sym`, `str`], [`fn`, [[`sym`, `a`]], [[`sym`, `AppendP`], [`sym`, `a`]]]], [[`sym`, `str`], [`sym`, `fnarg`]]]]

// ----------------------------
// todo : gross error msg.
// src/index.ts:218:7 - error TS2322: Type 'string[]' is not assignable to type '"AppendError"'.
//
// 218 const evallftest0: Eval<[lfInnerTest, [`prim`, `test''`]]> = [`prim`, `'+test'`]
//
// const evallftesterr: Eval<[lfInnerTest, [`prim`, `test''`]]> = [`prim`, `'+test'`]
// ----------------------------
const evallftest0: Eval<[lfInnerTest, [`prim`, `'test'`]]> = [`prim`, `'+test'`]

// ------------------------------------------
// the above is in the case of not recursive sexpr.
// -----------------------------------------

type Error1 = "RecEval1"
type Error2 = "RecEval2"
type Error3 = "RecEval3"
type Error4 = "RecEval4"
type Error5 = "RecEval5"

type RecEval<A, env> =
  env extends EnvLifo
  ? A extends [infer OPC, infer OPR]
    ? OPC extends SEXPR
      ? OPR extends SEXPR
        ? Eval<[RecEval<OPC, env> extends infer A ? A : never, RecEval<OPR, env> extends infer B ? B : never], env>
        : OPR extends ATOM
          ? Eval<[RecEval<OPC, env> extends infer A ? A : never, OPR], env>
          : Error1
    : OPC extends ATOM
      ? OPR extends SEXPR
        ? Eval<[OPC, RecEval<OPR, env> extends infer A ? A : never], env>
        : OPR extends ATOM
          ? Eval<[OPC, OPR], env>
          : Error2 : Error3 : Error4 : Error5

//test (no env)
const rbiTest:  RecEval<[[`sym`, `AppendP`], [`prim`, `'test'`]], []> = [`prim`,"'+test'"]
const rbiTest2: RecEval<[[`sym`, `AppendP`], [[`sym`, `AppendP`], [`prim`, `'test'`]]], []> = [`prim`,"'++test'"]
const rbiTest3: RecEval<[[`sym`, `AppendP`], [[`sym`, `AppendP`], [[`sym`, `AppendP`], [`prim`, `'test'`]]]], []> = [`prim`, "'+++test'"]
// test (with env)
const rbiTest4:  RecEval<[[`sym`, `AppendP`], [`sym`, `testsym`]], [[MakeVar<"testsym", `'test'`>]]> = [`prim`, "'+test'"]
const rbiTest5:  RecEval<[[`sym`, `AppendP`], [`sym`, `testsym`]], [[MakeVar<"testsym", `'test'`>], [MakeVar<"t", "'t'">], [MakeVar<"tttt", "'tttt'">]]> = [`prim`,"'+test'"]


// --------------------
// def / defn a scribble.
const defined: [`sym`, `test`] = null as any
const readdef: typeof defined = [`sym`, `test`] // null as any
// const readdef2: typeof defined = [`sym`, `tet`] // null as any
