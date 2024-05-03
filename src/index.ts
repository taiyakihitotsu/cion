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

type TDSDS = ["sym/AppendP", "'test'"]

type BuiltINerror1 = false
type BuiltINerror2 = false
type BuiltINerror3 = false
type BuiltINerror4 = false

// todo : ugly
type BuiltIN<A, env> =
  A extends [infer OPC, infer OPR]
  ? env extends Env
  ? OPC extends `sym/${infer U}`
    // todo : management built-ins.
    ? U extends `AppendP`
      ? AppendP<OPR>
      : BuiltINerror1 : BuiltINerror2 : BuiltINerror3 : BuiltINerror4

const buildinTest: BuiltIN<TDSDS, []> = "'+test'"
const decomTest: BuiltIN<Decompile<"(AppendP 'test')">, []> = "'+test'"

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
  env extends Env
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

// todo : fn resolve test

// todo : built in with env
// todo : making if pattern in built in
//   this is useful for a test in the case that
//   the 1st is SEXPR (not ATOM)
