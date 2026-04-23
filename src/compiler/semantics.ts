import type * as Bit from '../bit/index.js'
import type * as Decimal from '../decimal/index.js'
import type {TNil, TTrue, TFalse } from '../sexprtypes.js'
import type { ReadRational } from './util.js'

/**
Takes a single token and returns its corresponding internal AST representation.

1. This doesn't take to convert vector/map.
   SCompiler takes them.
2. 'if', 'let', 'fn' are reserved for syntaxes,
     so they aren't converted.
3. Every primitive should be wrapped as ['prim', V].
   These syntaxes are defined in `src/sexprtypes.ts`.
4. Assume a token is symbol if not matched.
   Returns wrapped as ['sym', S] if so.
*/
export type Symbolizer<
  Token extends string> =
  Token extends `${infer H}${infer _R}`
    ? H extends "'" | '"'
      ? [`prim`, Token]
    : ReadRational<Token> extends [`${infer F}${infer Fs}`, ...infer rD extends string[]]
      ? rD extends [`${infer _D}${infer _Ds}`]
        ? F extends '-'
          ? ['prim', [Bit.BitRevSign<Decimal.DecimalToBit<Fs>>, Decimal.DecimalToBit<`${_D}${_Ds}`>]]
        : ['prim', [Decimal.DecimalToBit<`${F}${Fs}`>, Decimal.DecimalToBit<`${_D}${_Ds}`>]]
      : F extends '-'
        ? ['prim', Bit.BitRevSign<Decimal.DecimalToBit<Fs>>]
      : ['prim', Decimal.DecimalToBit<`${F}${Fs}`>]
    : Token extends 'if' | 'let' | 'fn'
      ? Token
    : Token extends 'true'
      ? TTrue
    : Token extends 'false'
      ? TFalse
    : Token extends 'nil'
      ? TNil
    : [`sym`, Token]
  : never
