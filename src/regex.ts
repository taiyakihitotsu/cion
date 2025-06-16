import type { regexCompiler as rc } from './regex-compiler'
import type { regexEval as re } from './regex-eval'

// ------------------------
// -- [main] eval regexp
// ------------------------

type TapeType = rc.CompTape[]

export type ReadTape<
  String extends string
, TapeEnv extends {condition: string, tapes: TapeType}> =
  TapeEnv extends {condition: infer _cond, tapes: infer tape extends TapeType}
    ? re.TapeEval<String, tape>
  : re.Unmatch

export type TapeEvalLoop<
  S extends string
, Tape extends TapeType> =
  S extends ''
    ? re.Unmatch
  : re.TapeEval<S, Tape> extends [infer M extends string, infer Rest extends string, infer _List extends string[]]
    ? [M, Rest]
  : S extends `${infer _f}${infer sRest}`
    ? TapeEvalLoop<sRest, Tape>
  : never

export type RegexFind<
  String extends string
, Regex  extends string> =
  rc.Comp<Regex> extends { condition: infer Condition extends string
    , tapes: infer Tape extends TapeType}
    ? Condition extends '^' | '^$'
      ? re.TapeEval<String, Tape> extends [infer M extends string, infer Result extends string, infer _ extends string[]]
        ? (Result extends '' ? true : false) | (Condition extends '^$' ? false : true) extends false
          ? [M, Result]
        : re.Unmatch
      : re.Unmatch
    : Condition extends '$' | '^$'
      ? TapeEvalLoop<String, Tape> extends [infer M extends string, infer Result extends string]
        ? Result extends ''
          ? [M, Result]
        : re.Unmatch
      : re.Unmatch
    : TapeEvalLoop<String, Tape>
  : never

export type * as regex from './regex'
