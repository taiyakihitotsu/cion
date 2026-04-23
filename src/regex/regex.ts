import type { regexCompiler as rc } from './regex-compiler.js'
import type { regexEval as re } from './regex-eval.js'

type TapeType = rc.CompTape[]

export type TapeEvalLoop<
  S extends string
, Tape extends TapeType
, Consumed extends string = ''> =
  S extends ''
    ? re.Unmatch
  : re.TapeEval<S, Tape> extends [infer M extends string, infer Rest extends string, infer _List extends string[]]
    ? [Consumed, M, Rest]
  : S extends `${infer sFirst}${infer sRest}`
    ? TapeEvalLoop<sRest, Tape, `${Consumed}${sFirst}`>
  : never

/**
Returns a 3 member tuple if the second regex matches the first string.
Return an empty tuple if otherwise.

```typescript
type Res0 = RegexFind<'xszds', 's(z|d){0,2}s'> //=> ['x', 'szds', '']
type Res1 = RegexFind<'szdsye9r', '^s(z|d){0,2}s'> //=> ['', 'szds', 'ye9r']
type Res2 = RegexFind<'szdsye9r', '^s(z|d){0,2}s$'> //=> []
```
*/
export type RegexFind<
  String extends string
, Regex  extends string> =
  rc.Comp<Regex> extends { condition: infer Condition extends string
    , tapes: infer Tape extends TapeType}
    ? Condition extends '^' | '^$'
      ? re.TapeEval<String, Tape> extends [infer M extends string, infer Rest extends string, infer _ extends string[]]
        ? [Condition, Rest & ''] extends ['^$', never]
          ? re.Unmatch
        : ['', M, Rest]
      : re.Unmatch
    : Condition extends '$' | '^$'
      ? TapeEvalLoop<String, Tape> extends [infer Consumed extends string, infer M extends string, infer Rest extends string]
        ? Rest extends ''
          ? [Consumed, M, Rest]
        : re.Unmatch
      : re.Unmatch
    : TapeEvalLoop<String, Tape>
  : never

export type * as regex from './regex.js'
 
