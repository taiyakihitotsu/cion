import type * as Bit from './bit.js'
import type * as regexConst from './regex-const.js'

export type  tZero = '0000000000000000'
export type  tOne  = '0000000000000001'
export type  tTwo  = '0000000000000010'
export const vZero = '0000000000000000'
export const vOne  = '0000000000000001'
export const vTwo  = '0000000000000010'

export type Digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
export type Lower = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'
export type Upper = Uppercase<Lower>
export type WordChar = Digit | Lower | Upper | '_'
export type ASCII =
WordChar | ' ' | '!' | '"' | '#' | '$' | '%' | '&' | "'" | '(' | ')' |
  '*' | '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' |
  '[' | '\\' | ']' | '^' | '`' | '{' | '|' | '}' | '~'
export type NonDigit = Exclude<ASCII, Digit>
export type NonWordChar = Exclude<ASCII, WordChar>
export type NonLower = Exclude<ASCII, Upper>
export type NonUpper = Exclude<ASCII, Upper>
export type MetaChars = 'd' | 'w' | 'l' | 'u' | 'D' | 'W' | 'L' | 'U'

type Quantifiers =
'[' | ']' |
  '(' | ')' |
  '{' | '}' |
  '.' |
  '*' |
  '+' |
  '?' |
  '^' |
  '$' |
  '|' |
  '\\'

export type CharAt<
  S extends string
, Index extends string> =
  S extends `${infer F}${infer Rest}`
    ? Bit.BitIsZero<Index> extends true
      ? F
    : Rest extends ''
      ? ''
    : CharAt<Rest, Bit.BitDec<Index>>
  : ''

export type MatchChar<
  S extends string
, T extends string> =
  S extends `${infer sF}${infer sRest}`
    ? T extends '\\.'
      ? sF extends '.'
        ? true
      : false
    : T extends `\\d`
      ? sF extends Digit
        ? true
      : false
    : T extends `\\w`
      ? sF extends WordChar
        ? true
      : false
    : T extends `\\l`
      ? sF extends Lower
        ? true
      : false
    : T extends `\\u`
      ? sF extends Upper
        ? true
      : false
    : T extends `\\D`
      ? sF extends NonDigit
        ? true
      : false
    : T extends `\\W`
      ? sF extends NonWordChar
        ? true
      : false
    : T extends `\\L`
      ? sF extends NonLower
        ? true
      : false
    : T extends `\\U`
      ? sF extends NonUpper
        ? true
      : false
    : T extends `${infer tF}${infer tRest}`
      ? sRest extends ''
        ? tRest extends ''
          ? sF extends tF
            ? true
          : false
        : false
      : false
    : false
  : false

export type StrLen<
  S extends string
, N extends string = Bit.Zero> =
  S extends `${infer _}${infer Rest}`
    ? Rest extends ''
      ? Bit.BitInc<N>
    : StrLen<Rest, Bit.BitInc<N>>
  : tZero

export type SomeLen<
  S extends string
, T extends string> =
  StrLen<S> extends StrLen<T>
    ? true
  : false

export type StrTake<
  S extends string
, N extends string
, R extends string = ''> =
  (S extends '' ? true : false) & Bit.BitIsZero<N> extends true
    ? R
  : S extends `${infer f}${infer r}`
    ? StrTake<r, Bit.BitDec<N>, `${R}${f}`>
  : never

export type StrDrop<
  S extends string
, N extends string> =
  (S extends '' ? true : false) & Bit.BitIsZero<N> extends true
    ? S
  : S extends `${infer _f}${infer r}`
    ? StrDrop<r, Bit.BitDec<N>>
  : never

export type StrInter<
  S extends string
, N extends string
, M extends string> =
StrDrop<StrTake<S,Bit.BitInc<M>>, N>

// -------------------
// -- base of regexp
// -------------------

export type RegCut<
  S extends string> =
  S extends `${infer f}${infer s}${infer th}${infer rest}`
    ? [f, s] extends ['\\', MetaChars | Quantifiers]
      ? [`\\${s}`, `${th}${rest}`]
    : f extends `*`
      ? s extends `?`
        ? [`*?`, `${th}${rest}`]
      : [`*`, `${s}${th}${rest}`]
    : f extends `+`
      ? s extends `?`
        ? [`+?`, `${th}${rest}`]
      : [`+`, `${s}${th}${rest}`]
    : [f, s, th] extends [keyof regexConst.Words, '-', keyof regexConst.Words]
      ? [`${f}-${th}`, rest]
    : `${f}${s}` extends `[^`
      ? ['[^', `${th}${rest}`]
    : [f, `${s}${th}${rest}`]
  : S extends `${infer f}${infer s}${infer rest}`
    ? f extends '\\'
      ? s extends MetaChars | Quantifiers
        ? [`\\${s}`, `${rest}`]
      : never
    : f extends `*`
      ? s extends `?`
        ? [`*?`, `${rest}`]
      : [`*`, `${s}${rest}`]
    : f extends `+`
      ? s extends `?`
        ? [`+?`, `${rest}`]
      : [`+`, `${s}${rest}`]
    : `${f}${s}` extends `[^`
      ? ['[^', rest]
    : f extends '\\'
      ? [`\\${s}`, rest]
    : [f, `${s}${rest}`]
  : S extends `${infer f}${infer _rest}`
    ? f extends `*`
      ? [`*`, '']
    : f extends `+`
      ? [`+`, '']
    : [f, '']
  : never

export type RegFirstSplit<
  S extends string
, N extends 0|1> =
  S extends `${infer f}${infer rest}`
    ? S extends `\\${infer escapedFirst}${infer escapedRest}`
      ? escapedFirst extends '.'
        ? [`\\.`, escapedRest][N]
      : escapedFirst extends MetaChars
        ? [`\\${escapedFirst}`, escapedRest][N]
      : escapedFirst extends Quantifiers
        ? [`${escapedFirst}`, escapedRest][N]
      : never
    : [f, rest][N]
  : never

// [note]
// this works as ^.
//
// [note]
// This `StrSearchHead` matches `.` for any character.
// Again, `MatchChar` doesn't matches it but the metacharacters such as `\\d`.
export type StrSearchHead<
  S extends string
, Pattern extends string
, Complete extends string = ''
, Forward extends string = ''
, Flag extends '!wildcard' | 'wildcard' = 'wildcard'> =
  S extends `${infer sf}${infer srest}`
    ? ([RegFirstSplit<Pattern,0>, Flag] extends ['.', 'wildcard'] ? true : false) | MatchChar<sf, RegFirstSplit<Pattern,0>> extends false
      ? []
    : RegFirstSplit<Pattern,1> extends ''
      ? Complete extends 'complete'
        ? sf extends ''
          ? [`${Forward}${sf}`, srest]
        : []
      : [`${Forward}${sf}`, srest]
    : StrSearchHead<srest, RegFirstSplit<Pattern,1>, Complete, `${Forward}${sf}`, Flag>
  : never

// export type StrSearchHead<
//   S extends string
// , Pattern extends string
// , Complete extends string = ''
// , Forward extends string = ''
// , Flag extends '!wildcard' | 'wildcard' = 'wildcard'> =
//   S extends `${infer sf}${infer srest}`
//     ? ([RegFirstSplit<Pattern,0>, Flag] extends ['.', 'wildcard'] ? true : false) | MatchChar<sf, RegFirstSplit<Pattern,0>> extends false
//       ? []
//     : RegFirstSplit<Pattern,1> extends ''
//       ? Complete extends 'complete'
//         ? sf extends ''
//           ? [`${Forward}${sf}`, srest]
//         : []
//       : [`${Forward}${sf}`, srest]
//     : StrSearchHead<srest, RegFirstSplit<Pattern,1>, Complete, `${Forward}${sf}`, Flag>
//   : never

export type StrSearchAll<
  S extends string
, Pattern extends string
, Tag extends string = ''
, Forward extends string = ''
, Flag extends '!wildcard' | 'wildcard' = 'wildcard'> =
  S extends ''
    ? []
  : S extends `${infer F}${infer Rest}`
    ? Tag extends 'Head' | '^'
      ? StrSearchHead<S, Pattern, '^', Forward, Flag>
    : StrSearchHead<S, Pattern, '^', Forward, Flag> extends [infer Matched extends string, infer Rest extends string]
      ? Tag extends 'Tail' | '$'
        ? StrLen<S> extends StrLen<Pattern>
          ? [Matched, Rest]
        : StrSearchAll<Rest, Pattern, Tag, `${Forward}${F}`, Flag>
      : [Matched, Rest]
    : StrSearchAll<Rest, Pattern, Tag, `${Forward}${F}`, Flag>
  : 'not all'

export type * as StrUtil from './strutil.js'
