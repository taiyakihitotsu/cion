console.log('Hello World!!');


// Eval

type VarK = "name" | "value"

type Var = {
  [key in VarK] : string
}

type Env = Var[]

type NotMatch = "NotMatch"
const NotMatch = "NotMatch"

type MakeVar<N, V> = N extends string ? V extends string ? {name: N, value: V} : never : never 

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

// Note:
// This "Lifo" has no mean but just a naming.
type EnvLifo = Env[]

type Let<N,V,EnvLifo> = EnvLifo extends Env[] ? [...EnvLifo, [MakeVar<N,V>]] : never

type ReadLet<N, EnvLifo> = EnvLifo extends [...infer HS, infer L] ? L extends Env ? GetVar<N, L> extends NotMatch ? ReadLet<N, HS> : GetVar<N, L> : NotMatch : NotMatch

// test
type LetEnvLifo = [[MakeVar<"ss", "stringer">, MakeVar<"s", "string">]]
const letTest: Let<"sss", "str", LetEnvLifo> = [[{ name: "ss", value: "stringer" }, { name: "s", value: "string" }], [{ name: "sss", value: "str" }]]

const readLetTest: ReadLet<"s", Let<"sss", "str", LetEnvLifo>> = "string"
const readLetTest2:  ReadLet<"sss", Let<"sss", "str", LetEnvLifo>> = "str"
const readLetTest3:  ReadLet<"ssss", Let<"sss", "str", LetEnvLifo>> = NotMatch

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
type EvalError7 = "EvalError7"
type EvalError8 = "EvalError8"

// todo : there is no regex string template type,
//   so using taple type in a tempo.
type Sym  = [`sym`, string]
type Prim = [`prim`, string]
type Args = Sym[]
type Fn   = [`fn`, Args, Array<Sym | Prim | Fn | ATOM>]

type ATOM  = Sym | Prim | Fn
type SEXPR = Array<ATOM | SEXPR>

// test fn
const dectest: Fn = [`fn`, [[`sym`, `a`]], [[`sym`, `a`], [`sym`, `b`]]]

// todo : ugly
type Eval<A, env> =
  A extends [infer OPC, infer OPR]
  ? env extends EnvLifo
    ? OPC extends Fn
      ? OPC extends [`fn`, [[`sym`, infer S]], infer D]
	? OPR extends [`sym`, infer VV]
	  ? Eval<D, Let<S,ReadLet<VV, env>,env>>
          : OPR extends [`prim`, infer VVV]
            ? Eval<D, Let<S,VVV,env>> : EvalError5 : EvalError7
        : OPC extends [`sym`, infer U]
	// todo : management built-ins.
	? U extends `AppendP`
	  ? OPR extends [`sym`, infer V]
            // todo : in current,
            // if a sym isn't matched with env,
            // this returns appendP error, not env error (I hope.)
	    ? AppendP<ReadLet<V, env>>
	    : OPR extends [`prim`, infer W]
              ? AppendP<W>
	      : EvalError1 : EvalError2 : EvalError3 : EvalError4 : EvalError6

// test case
type TDSDS = [[`sym`, `AppendP`], [`prim`, `'test'`]]
type TDDDD = [[`sym`, `AppendP`], [`sym`, `str`]]
// test raw
const buildinTest: Eval<TDSDS, [[]]> = [`prim`, "'+test'"]
const buildinTest2: Eval<TDDDD, [[]]> = "AppendError"
const buildinTest3: Eval<TDDDD, [[MakeVar<`str`,`'strval'`>]]> = [`prim`,"'+strval'"]
const buildinTest4:  Eval<TDDDD, [[MakeVar<"str", "'strval'">], [MakeVar<"sstr", "'notstrval'">]]> = [`prim`, "'+strval'"]
const buildinTest5:  Eval<TDDDD, [[MakeVar<"sstr", "'notstrval'">], [MakeVar<"str", "'strval'">]]> = [`prim`, "'+strval'"]
// test fn

const builtinfntest: Eval<[[`fn`, [[`sym`, `str`]], TDDDD], [`prim`, `'test'`]], [[MakeVar<"str", "'strval'">]]> = [`prim`, "'+test'"]
const builtinfntest2: Eval<[[`fn`, [[`sym`, `str`]], TDDDD], [`prim`, `'test'`]], [[MakeVar<"aaa", "'aaa'">], [MakeVar<"str", "'strval'">]]> = [`prim`, "'+test'"]

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
        ? Eval<[RecEval<OPC, env>, RecEval<OPR, env>], env>
        : OPR extends ATOM
          ? Eval<[RecEval<OPC, env>, OPR], env>
          : Error1
    : OPC extends ATOM
      ? OPR extends SEXPR
        ? Eval<[OPC, RecEval<OPR, env>], env>
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
