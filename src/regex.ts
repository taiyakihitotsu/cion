import type * as Bit from './bit'
import type * as Decimal from './decimal'
import type { StrUtil as str } from './strutil'
import type { tZero, tOne, tTwo } from './strutil'
import { vZero, vOne, vTwo } from './strutil'

// -------------------------
// -- Regex Compiler
// -------------------------

type DtoB<Bit extends string> = Decimal.DecimalToBit<Bit>

export type MaxMinMatch = ['{.,.}', '{.,..}', '{..,..}', '{.,}', '{..,}', '{.}', '{..}', '{,.}','{,..}']

// ReadMinMax
//
// This returns `[]` as failure,
//   or [[BitStr, BitStr], 'times'] as success.
//  
export type ReadMinMax<
  S extends string> =
  MatchLoopForUnion<S, ['{.,.}', '{.,..}', '{..,..}'], 'char'> extends [[infer Match, infer Next], infer _Times]
    ? Match extends `{${infer n},${infer m}}`
      ? [[[DtoB<n>,DtoB<m>], 'times'], Next]
    : Match extends `{${infer na},${infer ma}${infer mb}}`
      ? [[[DtoB<na>,DtoB<`${ma}${mb}`>], 'times'], Next]
    : Match extends `{${infer nna}${infer nnb},${infer mma}${infer mmb}}`
      ? [[[DtoB<`${nna}${nnb}`>, DtoB<`${mma}${mmb}`>], 'times'], Next]
    : Match extends `{${infer pa},}`
      ? [[[DtoB<pa>,CMaxTime], 'times'], Next]
    : Match extends `{${infer pax}${infer pay},}`
      ? [[[DtoB<`${pax}${pay}`>,CMaxTime], 'times'], Next]
    : Match extends `{${infer zz}}`
      ? [[[DtoB<zz>,DtoB<`${zz}`>], 'times'], Next]
    : Match extends `{${infer zza}${infer zzb}}`
      ? [[[DtoB<`${zza}${zzb}`>,DtoB<`${zza}${zzb}`>], 'times'], Next]
    : []
  : []

type ReadEscape<S extends string> = S extends `\\${infer Escaped}${infer Rest}` ? Escaped extends str.MetaChars ? [`\\${Escaped}`, Rest] : [`${Escaped}`, Rest] : never

type CompFailed = []

// Compiler
//
// This function compiles a regex string into an array, which we call a `tape`.
// The final result is a record containing both conditions and the tape.
//
// Here is how each element of the tape is translated from each part of the regex:
//
// - A single character like `c` is compiled into ['c'], a one-element array.
// - A group like `(abc)` is compiled into ['abc'].
// - A union group like `(a|b)` is compiled into ['a', 'b'].
//   These are passed into a `MatchVecLoop`, where each element is read during matching.
// - A character set like `[ab]` is treated the same as a union group: ['a', 'b'].
//
// When `*` or `+` appears, the preceding element is packed into a tuple:
//   - The first item is the compiled array (e.g., ['abc', 'de'])
//   - The second item is the operator string: `*`.
//
//   For example, `(abc|de)*` first becomes ['abc', 'de'], then `*` is read,
//   so it becomes [['abc', 'de'], '*'] (example A).
//
//   If it's a `+`, another ['abc', 'de'] is appended the tape, before the tuple A is added to it.
//   So the tape ends with: [...tape, ['abc', 'de'], ['abc', 'de'], '*'].
//
// For `{n,m}`, the structure is similar to `*` or `+`, but the second element is:
//   [[n, m], 'times'] → See `ReadMinMax`.
// `?` is handled the same way, represented as [[tZero, tOne], 'times'].
//
// `^` and `$` are compiled into condition flags.
export type Comp<
  S extends string
, IsMerge extends string = ''
, MergeString extends string = ''
, MergeStack extends unknown[] = []
, Stack extends unknown[] = []
, tobeStack extends unknown[] = []
, condition extends string = ''> =
  S extends ''
    ? {condition: condition, tape: [...Stack, tobeStack]}
  : S extends `${infer sFirst}${infer sRest}`
    ? sFirst extends '^' | '$'
      ? Comp<sRest, IsMerge, MergeString, MergeStack, Stack, tobeStack, `${condition}${sFirst}`>
    : sFirst extends '('
      ? Comp<sRest, '(', '', MergeStack, [...Stack, tobeStack], [], condition>
    : sFirst extends ')'
      ? Comp<sRest, '', '', [...MergeStack, MergeString], [...Stack], [], condition>
    : sFirst extends '['
      ? Comp<sRest, '[', '', MergeStack, [...Stack, tobeStack], [], condition>
    : sFirst extends ']'
      ? Comp<sRest, '', '', [...MergeStack, MergeString], [...Stack], [], condition>
    : sFirst extends '+'
      ? Comp<sRest, '', '', [...(MergeStack extends [] ? [] : [MergeStack, '*'])], [...Stack, ...(MergeStack extends [] ? [] : [MergeStack]), ...(tobeStack extends [] ? [] : [tobeStack])], [...(tobeStack extends [] ? [] : [tobeStack, '*'])]>
    : sFirst extends '*'
      ? Comp<sRest, '', '', [...(MergeStack extends [] ? [] : [MergeStack, sFirst])], Stack, [...(tobeStack extends [] ? [] : [tobeStack, sFirst])], condition>
    : sFirst extends '?'
      ? Comp<sRest, '', '', [...(MergeStack extends [] ? [] : [MergeStack, [[tZero, tOne], 'times']])], Stack, [...(tobeStack extends [] ? [] : [tobeStack, [[tZero, tOne], 'times']])], condition>
    : sFirst extends '{'
      ? ReadMinMax<S> extends [infer FnPart, infer RestPart extends string]
        ? Comp<RestPart, '', '', [...(MergeStack extends [] ? [] : [MergeStack, FnPart])], Stack, [...(tobeStack extends [] ? [] : [tobeStack, FnPart])], condition>
      : CompFailed
    : IsMerge extends '('
      ? sFirst extends '|'
        ? Comp<sRest, '(', '', [...MergeStack, MergeString], Stack, [], condition>
      : Comp<sRest, '(', `${MergeString}${sFirst}`, MergeStack, Stack, [], condition>
    : IsMerge extends '['
      ? Comp<str.RegGet<S,1>, '[', str.RegGet<S,0>, [...MergeStack, ...(MergeString extends '' ? [] : [MergeString])], Stack, [], condition>
    : IsMerge extends ''
      ? sFirst extends '\\'
        ? ReadEscape<S> extends [infer sFirst extends string, infer srestRest extends string]
          ? Comp<srestRest, '', '', [], [...Stack, ...(tobeStack extends [] ? [] : [tobeStack]), ...(MergeStack extends [] ? [] : [MergeStack])], [sFirst], condition>
        : never
      : Comp<sRest, '', '', [], [...Stack, ...(tobeStack extends [] ? [] : [tobeStack]), ...(MergeStack extends [] ? [] : [MergeStack])], [sFirst], condition>
    : CompFailed
  : CompFailed



// ------------------------
// -- [main] eval regexp
// ------------------------

type RetStr<S, Ret = never> = S extends string ? S : Ret
type CMaxTime = '0000000000001111'
type SearchResult = [string, string]
type SearchTimeResult = [[string, string], string]

// ---------------------
// -- MatchLoop
// ---------------------
//
// This is the lower-level component of the Cion regex reader.
//
// Forward:
//   This represents a partially matched string passed from the calling function.
//   Each time this function is called,
//     `Forward` accumulates a matched part,
//     eventually returning the complete matched result.
//   `Forward` is passed in by the caller and accumulated here;
//     it is not modified by the caller itself.
//
// tag:
//   If `tag` is 'pattern',
//     the matched part is removed entirely (e.g., for patterns like `(abc)+`).
//   If `tag` is 'char',
//     only the first character is removed.
//   This enables suffix-priority matching.
//
// MaxTime / MinTime:
//   These are used for `{n,m}` repetition patterns.
//   Do not pass a value less than 1, especially not 0.
//
// i:
//   This represents how many times the pattern has been matched.
//   This is returned as the 2nd elemement of return
//     if this doesn't reach MaxTime, to restart with another pattern of union.
//
// Result:
//   This returns either:
//     - an empty array if matching fails
//     - or [[matchedString, remainingString], i] if successful.
//   In the success case:
//     - the first element of the first pair is the matched substring,
//     - the second is the remaining unmatched string,
//     - and the second element is the number of matches.
export type MatchLoop<
  S extends string
, Pattern extends string
, Tag extends 'pattern' | 'char' = 'pattern'
, MaxTime extends string = CMaxTime
, MinTime extends string = tOne
, Forward extends string = ''
, i extends string = tZero
, Result extends [] | SearchTimeResult = []
> =
  Bit.BitGTE<i, MaxTime> extends true
    ? Result
  : Bit.BitGTE<str.StrLen<S>, str.StrLen<Pattern>> extends true
    ? str.StrSearchAll<S, Pattern, '^', Forward> extends [infer Matched extends string, infer NextS extends string]
      ? S extends `${infer _SF}${infer SRest}`
        ? SRest extends ''
          ? str.StrSearchAll<S, Pattern, '^', `${Forward}${Matched}`> extends infer ssReturn
            ? ssReturn extends []
              ? []
            : ssReturn extends SearchResult 
              ? [[`${Forward}${Matched}`, NextS], Bit.BitInc<i>]
            : never
          : never
        : Bit.BitInc<i> extends infer j extends string
          ? MatchLoop<
            Tag extends 'pattern' ? RetStr<NextS> : SRest
          , Pattern
          , Tag
          , MaxTime
          , MinTime
	  , `${Forward}${Matched}`
          , j
          , [[`${Forward}${Matched}`, NextS], j]>
        : never
      : never
    : Bit.BitGTE<i, MinTime> extends true
      ? Bit.BitIsZero<i> extends true
        ? [[Forward, S], i]
      : Result
    : []
  : Result

// MatchLoopForUnion
//
// [note]
//   The term `Union` here refers to a part of regex,
//     not Union type in a type system.
//
// This function calls `MatchLoop` for each element of the tape.
// It correspondeds to an union pattern in the regex, which has been compiled to ['a', 'b'] (-> see `comp` )
export type MatchLoopForUnion<
  S extends string
, Pat extends string[]
, Tag extends 'pattern' | 'char' = 'pattern'
, Max extends string = CMaxTime
, Min extends string = tOne
, Forward extends string = ''> =
  Pat extends [infer First extends string, ...infer Rest extends string[]]
    ? MatchLoop<S, First, Tag, Max, Min, Forward> extends infer Result
      ? Result extends [[infer m extends string, infer r], infer t]
        ? Result
      : Rest extends []
        ? []
      : MatchLoopForUnion<S, Rest,Tag,Max,Min, Forward>
    : never
  : never

// recClimaxMatchLoopForUnion
//
// This function reads tape elements against a given string.
//
// This function calls `MatchLoopForUnion`, which returns a `SearchTimeResult`.
// If the match count does not reach `maxTime`, but satisfies `minTime`,
//   the result's match count is passed as the next `minTime`
//   to implement the `{n,m}` repetition pattern.
type ClimaxFailed = []
type recClimaxMatchLoopForUnion<
  String extends string
, groupTape extends string[]
, maxTime extends string = CMaxTime
, minTime extends string = tOne
, Forward extends string = ''
, currentTime extends string = tZero
, Result extends [[string, string], string] | [] = []> =
  String extends ''
    ? Result
  : Bit.BitGTE<currentTime, maxTime> extends true
    ? Result
  : MatchLoopForUnion<String, groupTape, 'pattern', maxTime, minTime, Forward> extends [[infer Matched extends string, infer NextString extends string], infer DoneTime extends string] & infer wholeResult extends SearchTimeResult
    ? Bit.BitAdd<DoneTime,currentTime> extends infer NextTime
      ? Bit.BitEq<maxTime,NextTime> extends true
        ? [[Matched, NextString], DoneTime]
      : recClimaxMatchLoopForUnion<NextString, groupTape, maxTime, minTime, Matched, Bit.BitAdd<DoneTime,currentTime>, [[Matched, NextString], DoneTime]>
    : never
  : Bit.BitLTE<minTime, currentTime> extends true
    ? Result
  : ClimaxFailed

// ClimaxMatchLoopForUnion
//
// This is a wrapper of `recClimaxMatchLoopForUnion`
//
// [Note]
//   BitZero isn't accepted so we need to inc minTime if zero.
export type ClimaxMatchLoopForUnion<
  String extends string
, groupTape extends string[]
, maxTime extends string
, minTime extends string
, Forward extends string = ''> =
  Bit.BitLT<maxTime, minTime> extends true
    ? never
  : Bit.BitLT<minTime, tZero> extends true
    ? never
  : Bit.BitIsZero<minTime> extends true
    ? recClimaxMatchLoopForUnion<String, groupTape, maxTime, Bit.BitInc<minTime>, Forward>
  : recClimaxMatchLoopForUnion<String, groupTape, maxTime, minTime, Forward>

type JustSymbolScene  = [string]
type UnionSymbolScene = string[]
type FnTimesSign = [[string, string], 'times']
type FnScene = [string[], (string | FnTimesSign)]
type TapeType = (JustSymbolScene|UnionSymbolScene|FnScene|FnTimesSign)[] 
type RTape<T> = T extends TapeType ? T : never
type FnSigns = '*' | '?' | FnTimesSign
type SearchFailed = []


// TapeEval
//
// This function evaluates a compiled regex tape against an input string.
//
// [note]
// This does not represent the full process of regex string matching.
// If this function is called only once, it behaves like matching a regex with `^`.
//
// [note]
// `+` is internally expanded into `*`, during compilation.
//
// [Note]
//   Patterns like `ss*s` should ideally be merged into `ss*`,  
//   just like `(xyz)*xyz` should become `(xyz)*`.
//   This optimization was abandoned to allow greedy behavior,
//   so such patterns are considered illegal in this implementation.
export type TapeEval<
  String extends string
, Tape extends TapeType = []
, Forward extends string = ''> =
  Tape extends []
    ? [Forward, String]
  : Tape extends [infer firstTape, ...infer restTape]
    ? firstTape extends JustSymbolScene & [infer Pat extends string]
      ? MatchLoop<String, Pat, 'pattern', tOne, tOne, Forward> extends [[infer Matched extends string, infer Next extends string], infer _Time]
        ? TapeEval<Next, RTape<restTape>, Matched>
      : SearchFailed
    : firstTape extends UnionSymbolScene & infer Pats extends string[]
      ? MatchLoopForUnion<String, Pats, 'pattern', tOne, tOne, Forward> extends [[infer Matched extends string, infer Next extends string], infer _Time]
        ? TapeEval<Next, RTape<restTape>, Matched>
      : SearchFailed
    : firstTape extends [infer groupTape extends string[], infer Fn extends FnSigns]
      ? Fn extends '*'
        ? MatchLoopForUnion<String, groupTape, 'pattern', CMaxTime, tOne, Forward> extends [[infer Matched extends string, infer Next extends string], infer _Time]
          ? TapeEval<Next, RTape<restTape>, Matched>
        : TapeEval<String, RTape<restTape>, Forward>
      : Fn extends '?'
        ? MatchLoopForUnion<String, groupTape, 'pattern', tOne, tOne, Forward> extends [[infer Matched extends string, infer Next extends string], infer _Time]
          ? TapeEval<Next, RTape<restTape>, Matched>
        : TapeEval<String, RTape<restTape>, Forward>
      : Fn extends FnTimesSign & [ [ infer minTime extends string
                                   , infer maxTime extends string]
                                 , 'times' ]
        ? ClimaxMatchLoopForUnion<String, groupTape, maxTime, minTime, Forward> extends [[infer Matched extends string, infer Next extends string], infer _Time]
          ? TapeEval<Next, RTape<restTape>, Matched>
        : true extends Bit.BitIsZero<minTime>
          ? TapeEval<String, RTape<restTape>, Forward>
        : SearchFailed
      : never
    : never
  : never

export type ReadTape<
  String extends string
, TapeEnv extends {condition: string, tape: TapeType}> =
  TapeEnv extends {condition: infer cond, tape: infer tape extends TapeType}
    ? TapeEval<String, tape>
  : []

export type TapeEvalLoop<
  S extends string
, Tape extends TapeType> =
  S extends ''
    ? []
  : TapeEval<S, Tape> extends infer R
    ? R extends [] 
      ? S extends `${infer _f}${infer sRest}`
        ? TapeEvalLoop<sRest, Tape>
      : never
    : R
  : never

export type RegexFind<
  String extends string
, Regex  extends string> =
  Comp<Regex> extends {condition: infer Condition extends string
		      , tape: infer Tape extends TapeType}
    ? Condition extends '^' | '^$'
      ? ReadTape<String, {condition: Condition, tape: Tape}>
    : Condition extends '$' | '^$'
      ? TapeEvalLoop<String, Tape> extends [infer M extends string, infer Result extends string]
        ? Result extends ''
          ? [M, Result]
        : []
      : []
    : TapeEvalLoop<String, Tape>
  : never

export type * as regex from './regex'
