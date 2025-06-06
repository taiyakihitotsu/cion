import type Bit from './bit'
import type Decimal from './decimal'
import type { strutil as str } from './strutil'
import type { tZero, tOne, tTwo } from './strutil'
import { vZero, vOne, vTwo } from './strutil'

export namespace regex {

// ------------------------
// -- [main] eval regexp
// ------------------------

type RetStr<S, Ret = never> = S extends string ? S : Ret
type InitEnv = {pattern: undefined, sign: undefined}

type CMaxTime = '0000000000001111'

// tag: 
//   If tag is pattern, matched parts are removed by each call, to match (abc)+ or so.
//   tag is char, the head only is removed.
//   this acts as suffix priority matching.
// MinTime: 
//   Don't pass a number less than 1, especially 0.
// i:
//   Times info is returned as the 2nd.
//     this says how many times a passed pattern is occured,
//     even though tag is 'char'.
//   This i should be the same of MaxTime
//     only if you don't pass the union,
//       which is expressed as a vector in actual.
//     (see the test cases.)
export type MatchLoop<
  S extends string
, Pattern extends string
, Tag extends 'pattern' | 'char' = 'pattern'
, MaxTime extends string = CMaxTime
, MinTime extends string = tOne
, i extends string = tZero
, Result extends [] | [[string, string], string] = []> =
  Bit.BitGTE<i, MaxTime> extends true
    ? Result
  : Bit.BitGTE<str.StrLen<S>, str.StrLen<Pattern>> extends true
    ? str.StrSearchAll<S, Pattern, '^'> extends [infer Matched, infer NextS] & [string, string] & infer Whole
      ? S extends `${infer _SF}${infer SRest}`
        ? SRest extends ''
          ? str.StrSearchAll<S, Pattern, '^'> extends infer ssReturn
            ? ssReturn extends []
              ? []
            : [ssReturn,Bit.BitInc<i>]
          : never
        : Bit.BitInc<i> extends infer j extends string
          ? MatchLoop<
            Tag extends 'pattern' ? RetStr<NextS> : SRest
          , Pattern
          , Tag
          , MaxTime
          , MinTime
          , j //Bit.BitInc<i>
          , [[RetStr<Matched>, RetStr<NextS>], j]>
        : never
      : never
    : Bit.BitGTE<i, MinTime> extends true
      ? Bit.BitIsZero<i> extends true
        ? [['', S], i]
      : Result
    : []
  : Result

type MatchLoopVec<
  S extends string
, Pat extends string[]
, Tag extends 'pattern' | 'char' = 'pattern'
, Max extends string = CMaxTime
, Min extends string = tOne> =
  Pat extends [infer First extends string, ...infer Rest extends string[]]
    ? MatchLoop<S, First, Tag, Max, Min> extends infer Result
      ? Result extends []
        ? Rest extends []
          ? Result
        : MatchLoopVec<S, Rest,Tag,Max,Min>
      : Result
    : never
  : never

// test
const xtest_mloop0: MatchLoopVec<'xxxxx', ['xx'], 'char', tOne> = [['xx', 'xxx'], vOne]
const xtest_mloop1: MatchLoopVec<'xxxxx', ['xx'], 'pattern'> = [['xx', 'x'], vTwo]
const xtest_mloop1a: MatchLoopVec<'xxxxx', ['xx'], 'pattern', tOne> = [['xx', 'xxx'], vOne]
const xtest_mloop2: MatchLoopVec<'xxxxx', ['x'], 'char'>  = [['x', ''], '0000000000000101']
const xtest_mloop3: MatchLoopVec<'xxxxxx', ['x'], 'char'> = [['x', ''], '0000000000000110']
const xtest_mloop2a: MatchLoopVec<'xxxxx', ['x'], 'char', Bit.BitInc<tOne>, tOne> = [['x', 'xxx'], vTwo]
const xtest_mloop3a: MatchLoopVec<'xxxxxx', ['x'], 'char', Bit.BitInc<Bit.BitInc<tOne>>, tOne> = [['x', 'xxx'], '0000000000000011']
const xtest_mloop4: MatchLoopVec<'xxxyxxx', ['x'], 'char'> = [['x', 'yxxx'], '0000000000000011']
const xtest_mloop5: MatchLoopVec<'xxxyxxx', ['x'], 'char', Bit.BitInc<tOne>> = [['x', 'xyxxx'], vTwo]
// group
const ytest_mloop0: MatchLoopVec<'xxxxx', ['y','xx'], 'char', tOne>     = [['xx', 'xxx'], vOne]
const ytest_mloop1: MatchLoopVec<'xxxxx', ['y','xx'], 'pattern'>       = [['xx', 'x'], vTwo]
const ytest_mloop1a: MatchLoopVec<'xxxxx', ['y','xx'], 'pattern', tOne> = [['xx', 'xxx'], vOne]
const ytest_mloop2: MatchLoopVec<'xxxxx', ['y','x'], 'char'>  = [['x', ''], "0000000000000101"]
const ytest_mloop3: MatchLoopVec<'xxxxxx', ['y','x'], 'char'> = [['x', ''], "0000000000000110"]
const ytest_mloop2a: MatchLoopVec<'xxxxx', ['y','x'], 'char', Bit.BitInc<tOne>, tOne> = [['x', 'xxx'], vTwo]
const ytest_mloop3a: MatchLoopVec<'xxxxxx', ['y','x'], 'char', Bit.BitInc<Bit.BitInc<tOne>>, tOne> = [['x', 'xxx'], "0000000000000011"]
const ytest_mloop4: MatchLoopVec<'xxxyxxx', ['y', 'x'], 'char'> = [['x', 'yxxx'], "0000000000000011"]
const ytest_mloop5: MatchLoopVec<'xxxyxxx', ['y','x'], 'char', Bit.BitInc<tOne>> = [['x', 'xyxxx'], vTwo]

type MaxMinMatch = ['{.,.}', '{.,..}', '{..,..}', '{.,}', '{..,}', '{.}', '{..}', '{,.}','{,..}']
const testmaxmin0a: MatchLoopVec<'{n,m}rest', MaxMinMatch, 'char'>[0]   = ['{n,m}', 'rest']
const testmaxmin1a: MatchLoopVec<'{n,mm}rest', MaxMinMatch, 'char'>[0]  = ['{n,mm}', 'rest']
const testmaxmin2a: MatchLoopVec<'{nn,mm}rest', MaxMinMatch, 'char'>[0] = ['{nn,mm}', 'rest']
// they are not used though.
const testmaxmin0b: MatchLoopVec<'{,m}rest', MaxMinMatch, 'char'>[0]  = ['{,m}', 'rest']
const testmaxmin1b: MatchLoopVec<'{,mm}rest', MaxMinMatch, 'char'>[0] = ['{,mm}', 'rest']
const testmaxmin0c: MatchLoopVec<'{n,}rest', MaxMinMatch, 'char'>[0]  = ['{n,}', 'rest']
const testmaxmin1c: MatchLoopVec<'{nn,}rest', MaxMinMatch, 'char'>[0] = ['{nn,}', 'rest']
const testmaxmin0d: MatchLoopVec<'{n}rest', MaxMinMatch, 'char'>[0]   = ['{n}', 'rest']
const testmaxmin1d: MatchLoopVec<'{nn}rest', MaxMinMatch, 'char'>[0]  = ['{nn}', 'rest']

type ReadMinMax<
  S extends string> =
  MatchLoopVec<S, ['{.,.}', '{.,..}', '{..,..}'], 'char'> extends [[infer Match, infer Next], infer _Times]
    ? Match extends `{${infer n},${infer m}}`
      ? [[[Decimal.DecimalToBit<n>,Decimal.DecimalToBit<m>], 'times'], Next]
    : Match extends `{${infer na},${infer ma}${infer mb}}`
      ? [[[Decimal.DecimalToBit<na>,Decimal.DecimalToBit<`${ma}${mb}`>], 'times'], Next]
    : Match extends `{${infer nna}${infer nnb},${infer mma}${infer mmb}}`
      ? [[[Decimal.DecimalToBit<`${nna}${nnb}`>, Decimal.DecimalToBit<`${mma}${mmb}`>], 'times'], Next]
    : Match extends `{${infer pa},}`
      ? [[[Decimal.DecimalToBit<pa>,CMaxTime], 'times'], Next]
    : Match extends `{${infer pax}${infer pay},}`
      ? [[[Decimal.DecimalToBit<`${pax}${pay}`>,CMaxTime], 'times'], Next]
    : Match extends `{${infer zz}}`
      ? [[[Decimal.DecimalToBit<zz>,Decimal.DecimalToBit<`${zz}`>], 'times'], Next]
    : Match extends `{${infer zza}${infer zzb}}`
      ? [[[Decimal.DecimalToBit<`${zza}${zzb}`>,Decimal.DecimalToBit<`${zza}${zzb}`>], 'times'], Next]
    : []
  : []
// test
const testmaxminr0: ReadMinMax<`{0,1}rest`> = [[['0000000000000000', '0000000000000001'], 'times'], 'rest']
const testmaxminr1: ReadMinMax<`{0,15}rest`> = [[['0000000000000000', '0000000000001111'], 'times'], 'rest']
const testmaxminr2: ReadMinMax<`{15,16}rest`> = [[['0000000000001111', '0000000000010000'], 'times'], 'rest']
const testmaxminr3: ReadMinMax<`{0,1rest`> = []

type CompFailed = []
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
    : sFirst extends '*' | '?'
      ? Comp<sRest, '', '', [...(MergeStack extends [] ? [] : [MergeStack, sFirst])], Stack, [...(tobeStack extends [] ? [] : [tobeStack, sFirst])], condition>
    : sFirst extends '{'
      ? ReadMinMax<S> extends [infer FnPart, infer RestPart extends string]
        ? Comp<RestPart, '', '', [...(MergeStack extends [] ? [] : [MergeStack, FnPart])], Stack, [...(tobeStack extends [] ? [] : [tobeStack, FnPart])], condition>
      : CompFailed
    : IsMerge extends '('
      ? sFirst extends '|'
        ? Comp<sRest, '(', '', [...MergeStack, MergeString], Stack, [], condition>
      : Comp<sRest, '(', `${MergeString}${sFirst}`, MergeStack, Stack, [], condition>
    : IsMerge extends '['
      ? Comp<sRest, '[', sFirst, [...MergeStack, ...(MergeString extends '' ? [] : [MergeString])], Stack, [], condition>
    : IsMerge extends ''
      ? Comp<sRest, '', '', [], [...Stack, ...(tobeStack extends [] ? [] : [tobeStack]), ...(MergeStack extends [] ? [] : [MergeStack])], [sFirst], condition>
    : CompFailed
  : CompFailed

type ClimaxFailed = []
type recClimaxMatchLoopVec<
  String extends string
, groupTape extends string[]
, maxTime extends string = CMaxTime
, minTime extends string = tOne
, currentTime extends string = tZero
, Result extends [[string, string], string] | [] = []> =
  String extends ''
    ? Result
  : Bit.BitGTE<currentTime, maxTime> extends true
    ? Result
  : MatchLoopVec<String, groupTape, 'pattern', maxTime, minTime> extends [[infer _Matched extends string, infer NextString extends string], infer DoneTime extends string] & infer wholeResult
    ? Bit.BitAdd<DoneTime,currentTime> extends infer NextTime
      ? Bit.BitEq<maxTime,NextTime> extends true
        ? wholeResult
      : recClimaxMatchLoopVec<NextString, groupTape, maxTime, minTime, Bit.BitAdd<DoneTime,currentTime>,[[_Matched, NextString], DoneTime]>
    : never
  : Bit.BitLTE<minTime, currentTime> extends true
    ? Result
  : ClimaxFailed
  

type ClimaxMatchLoopVec<
  String extends string
, groupTape extends string[]
, maxTime extends string
, minTime extends string> =
  Bit.BitLT<maxTime, minTime> extends true
    ? never
  : Bit.BitLT<minTime, tZero> extends true
    ? never
  : Bit.BitIsZero<minTime> extends true
    ? recClimaxMatchLoopVec<String, groupTape, maxTime, Bit.BitInc<minTime>>
  : recClimaxMatchLoopVec<String, groupTape, maxTime, minTime>

// test
type aaaaaaaaa = Comp<'s(y|x){0,2}s'>
const aaaa: ClimaxMatchLoopVec<'xxsssssss', ['y', 'x'], '0000000000000010', '0000000000000001'> = [['x', 'sssssss'], vTwo]
const aaaab: ClimaxMatchLoopVec<'xysssssss', ['y', 'x'], '0000000000000010', '0000000000000001'> = [['y', 'sssssss'], vOne]
const aaaabbbb: ClimaxMatchLoopVec<'xsssssss', ['y', 'x'], '0000000000000010', '0000000000000001'> = [['x', 'sssssss'], vOne] 
const aaaabbbbc: ClimaxMatchLoopVec<'xsssssss', ['y', 'z'], '0000000000000010', '0000000000000001'> = [] 

type JustSymbolScene  = [string]
type UnionSymbolScene = string[]
type FnTimesSign = [[string, string], 'times']
type FnScene = [string[], (string | FnTimesSign)]
type TapeType = (JustSymbolScene|UnionSymbolScene|FnScene|FnTimesSign)[] 
type RTape<T> = T extends TapeType ? T : never
type FnSigns = '*' | '?' | FnTimesSign
type SearchFailed = []
//
// TapeEval
//   This reads a compiled regex.
//
// [note]
//    plus is expanded to *, with comp.
//
// [note]
//   ss*s should be merged to ss*, the same as (xyz)*xyz to (xyz)*
//   I abandoned this task as I admit an greedy, so this regex has been illegal.
//
// [note]
// This TapeEval works as all of tape has '^'.
// see this case (in test/regex.ts):
// // const evaltesttms_comp0vbz3e: regex.TapeEval<'esssyysssssss', regex.Comp<'s(z|d){0,2}s'>> = []
// 
export type TapeEval<
  String extends string
, Tape extends TapeType = []
, LastMatch extends string = ''> =
  Tape extends []
    ? [LastMatch, String]
  : Tape extends [infer firstTape, ...infer restTape]
    ? firstTape extends JustSymbolScene & [infer Pat extends string]
      ? MatchLoop<String, Pat, 'pattern', tOne> extends [[infer Matched extends string, infer Next extends string], infer _Time]
        ? TapeEval<Next, RTape<restTape>, Matched>
      : SearchFailed
    : firstTape extends UnionSymbolScene & infer Pats extends string[]
      ? MatchLoopVec<String, Pats, 'pattern', tOne> extends [[infer Matched extends string, infer Next extends string], infer _Time]
        ? TapeEval<Next, RTape<restTape>, Matched>
      : SearchFailed
    : firstTape extends [infer groupTape extends string[], infer Fn extends FnSigns]
      ? Fn extends '*'
        ? MatchLoopVec<String, groupTape, 'pattern', CMaxTime> extends [[infer Matched extends string, infer Next extends string], infer _Time]
          ? TapeEval<Next, RTape<restTape>, Matched>
        : TapeEval<String, RTape<restTape>, LastMatch>
      : Fn extends '?'
        ? MatchLoopVec<String, groupTape, 'pattern', tOne> extends [[infer Matched extends string, infer Next extends string], infer _Time]
          ? TapeEval<Next, RTape<restTape>, Matched>
        : TapeEval<String, RTape<restTape>, LastMatch>
      : Fn extends FnTimesSign & [ [ infer minTime extends string
                                   , infer maxTime extends string]
                                 , 'times' ]
        ? ClimaxMatchLoopVec<String, groupTape, maxTime, minTime> extends [[infer Matched extends string, infer Next extends string], infer _Time]
          ? TapeEval<Next, RTape<restTape>, Matched>
        : true extends Bit.BitIsZero<minTime>
          ? TapeEval<String, RTape<restTape>, LastMatch>
        : SearchFailed
      : never
    : never
  : never

export type ReadTape<
  String extends string
, TapeEnv extends {condition: string, tape: TapeType}
, LastMatch extends string = ''> =
  TapeEnv extends {condition: infer cond, tape: infer tape extends TapeType}
    ? TapeEval<String, tape>
  : []

} export default regex
