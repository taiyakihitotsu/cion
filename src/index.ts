console.log('Hello World!!');


// Eval

type SymK = "name" | "value"

type Sym = {
  [key in SymK] : string
}

type Env = Sym[]

type NotMatch = false
const NotMatch = false

type MakeSym<N, V> = N extends string ? V extends string ? {name: N, value: V} : never : never 

type GetSym<T, E> = 
  E extends Env
  ? E extends [infer U, ...infer R]
    ? U extends Sym
      ? U["name"] extends T
        ? U["value"]
        : GetSym<T, R> : NotMatch : NotMatch : NotMatch

// test
const getSymTest: GetSym<"s", [MakeSym<"ss", "stringer">, MakeSym<"s", "string">]> = "string"
const getSymTest2: GetSym<"ss", [MakeSym<"ss", "stringer">, MakeSym<"s", "string">]> = "stringer"
const EvalTest3: GetSym<"sss", [MakeSym<"ss", "stringer">, MakeSym<"s", "string">]> = NotMatch

//------------------------------------------------

// Let
// Env in Let must be a list of LIFO.

// Note:
// This "Lifo" has no mean but just a naming.
type EnvLifo = Env[]

type Let<N,V,EnvLifo> = EnvLifo extends Env[] ? [...EnvLifo, [MakeSym<N,V>]] : never

type ReadLet<N, EnvLifo> = EnvLifo extends [...infer HS, infer L] ? L extends Env ? GetSym<N, L> extends NotMatch ? ReadLet<N, HS> : GetSym<N, L> : NotMatch : NotMatch

// test
type LetEnvLifo = [[MakeSym<"ss", "stringer">, MakeSym<"s", "string">]]
const letTest: Let<"sss", "str", LetEnvLifo> = [[{ name: "ss", value: "stringer" }, { name: "s", value: "string" }], [{ name: "sss", value: "str" }]]

const readLetTest: ReadLet<"s", Let<"sss", "str", LetEnvLifo>> = "string"
const readLetTest2:  ReadLet<"sss", Let<"sss", "str", LetEnvLifo>> = "str"
const readLetTest3:  ReadLet<"ssss", Let<"sss", "str", LetEnvLifo>> = NotMatch

//-----------------------------------------

// Def
// 
// Note: Ignore duplicate def (in current)
//   to fix it, making error handling type.

type Def<N,V,EnvLifo> = EnvLifo extends [infer U, ...infer R] ? U extends Env ? [[MakeSym<N,V>, ...U], ...R] : never :never

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

// todo
type CompileError = false

type Decompile<S> = S extends `(${infer U} ${infer R})` ? [`sym/${U}`, R] : CompileError

type AppendP<S> = S extends `'${infer U}'` ? `'+${U}'` : never

const appendTest: AppendP<"'test'"> = "'+test'"

type BuiltINerror1 = "BuiltINerror1"
type BuiltINerror2 = "BuiltINerror2"
type BuiltINerror3 = "BuiltINerror3"
type BuiltINerror4 = "BuiltINerror4"
type BuiltINerror5 = "BuiltINerror5"
type BuiltINerror6 = "BuiltINerror6"
type BuiltINerror7 = "BuiltINerror7"
type BuiltINerror8 = "BuiltINerror8"

type Decompiled = string[] // todo : rec

// todo : integrate
// todo : Sym -> Sym[] / Env
// todo : decompiled type sort.
type FnDecompiled = [`fn`, string, Decompiled]

// test
const dectest: FnDecompiled = [`fn`, `a`, [`sym/AppendP`, `sym/a`]]

// todo : ugly
type BuiltIN<A, env> =
  A extends [infer OPC, infer OPR]
  ? env extends EnvLifo
    ? OPC extends FnDecompiled
      ? OPC extends [`fn`, infer S, infer D]
	// todo : integrate
	? OPR extends `sym/${infer VV}`
	  ? BuiltIN<D, Let<S,ReadLet<VV, env>,env>>
	  : BuiltIN<D, Let<S,OPR,env>> : BuiltINerror5
    // todo : defining sym in this way, good or bad??
    : OPC extends `sym/${infer U}`
      // todo : management built-ins.
      ? U extends `AppendP`
	? OPR extends `sym/${infer V}`
	  ? AppendP<ReadLet<V, env>>
	  : AppendP<OPR>
	    : BuiltINerror1 : BuiltINerror2 : BuiltINerror3 : BuiltINerror4

// test
type TDSDS = ["sym/AppendP", "'test'"]
type TDDDD = ["sym/AppendP", "sym/str"]
// test raw
const buildinTest: BuiltIN<TDSDS, [[]]> = "'+test'"
const decomTest: BuiltIN<Decompile<"(AppendP 'test')">, [[]]> = "'+test'"
// test with env
const buildinTest2: BuiltIN<TDDDD, [[MakeSym<"str", "'strval'">]]> = "'+strval'"
const buildinTest3:  BuiltIN<TDDDD, [[MakeSym<"str", "'strval'">], [MakeSym<"sstr", "'notstrval'">]]> = "'+strval'"
const buildinTest4:  BuiltIN<TDDDD, [[MakeSym<"sstr", "'notstrval'">], [MakeSym<"str", "'strval'">]]> = "'+strval'"
// test fn
const builtinfntest: BuiltIN<[[`fn`, `str`, TDDDD], "'test'"], [[MakeSym<"str", "'strval'">]]> = "'+test'"
const builtinfntest2: BuiltIN<[[`fn`, `str`, TDDDD], "'test'"], [[MakeSym<"aaa", "'aaa'">], [MakeSym<"str", "'strval'">]]> = "'+test'"

// ------------------------------------------
// the above is in the case of not recursive sexpr.
// -----------------------------------------

type ATOM  = string
// type SEXPR = [ATOM | SEXPR, ATOM | SEXPR]
type SEXPR = Array<ATOM | SEXPR>

type Error1 = false
type Error2 = false
type Error3 = false
type Error4 = false
type Error5 = false

type RecBuiltIN<A, env> =
  env extends EnvLifo
  ? A extends [infer OPC, infer OPR]
    ? OPC extends SEXPR
      ? OPR extends SEXPR
        ? BuiltIN<[RecBuiltIN<OPC, env>, RecBuiltIN<OPR, env>], env>
        : OPR extends ATOM
          ? BuiltIN<[RecBuiltIN<OPC, env>, OPR], env>
          : Error1
    : OPC extends ATOM
      ? OPR extends SEXPR
        ? BuiltIN<[OPC, RecBuiltIN<OPR, env>], env>
        : OPR extends ATOM
          ? BuiltIN<[OPC, OPR], env>
          : Error2 : Error3 : Error4 : Error5

//test

const rbiTest:  RecBuiltIN<["sym/AppendP", "'test'"], []> = "'+test'"
const rbiTest2: RecBuiltIN<["sym/AppendP", ["sym/AppendP", "'test'"]], []> = "'++test'"
const rbiTest3: RecBuiltIN<["sym/AppendP", ["sym/AppendP", ["sym/AppendP", "'test'"]]], []> = "'+++test'"
const rbiTest4:  RecBuiltIN<["sym/AppendP", "sym/testsym"], [[MakeSym<"testsym", "'test'">]]> = "'+test'"
const rbiTest5:  RecBuiltIN<["sym/AppendP", "sym/testsym"], [[MakeSym<"testsym", "'test'">], [MakeSym<"t", "'t'">], [MakeSym<"tttt", "'tttt'">]]> = "'+test'"
// --------------------------------------------------------
// [NOTE]
// Pitfall if forgot ,comma.
// --------------------------------------------------------
// src/index.ts:161:94 - error TS2538: Type '{ name: "t"; value: "'t'"; }' cannot be used as an index type.
//
// 161 const rbiTest5:  RecBuiltIN<["sym/AppendP", "sym/testsym"], [[MakeSym<"testsym", "'test'">] [MakeSym<"t", "'t'">]]> = "'+test'"
// ---------------------------------------------------------
        


// todo : fn resolve test

// todo : built in with env
// todo : making if pattern in built in
//   this is useful for a test in the case that
//   the 1st is SEXPR (not ATOM)
