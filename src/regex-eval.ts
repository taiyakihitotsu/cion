import type { regexCompiler as rc } from './regex-compiler'
import type { StrUtil as str } from './strutil'

export type Unmatch = []

export type OrMatch<
  S extends string
, Pat extends string[]
, Forward extends string
, Flag extends '!wildcard' | 'wildcard' = 'wildcard'> =
  Pat extends []
    ? Unmatch
  : Pat extends [infer First extends string, ...infer Rest extends string[]]
    ? str.StrSearchAll<S, First, '^', Forward, Flag> extends [infer M extends string, infer Re extends string]
      ? [M, Re]
    : OrMatch<S, Rest, Forward, Flag>
  : Unmatch


export type OrGroupMatch<
  S extends string
, Pat extends rc.CompTape[]
, Forward extends string> =
  Pat extends []
    ? Unmatch
  : Pat extends [infer First extends rc.CompTape, ...infer Rest extends rc.CompTape[]]
    ? _TapeEval<S, First, Forward, []> extends [infer M extends string, infer Re extends string, infer Tp extends rc.CompTape]
      ? [M, Re, Tp]
    : OrGroupMatch<S, Rest, Forward>
  : Unmatch


export type AstaMatch<
  S extends string
, Pat extends rc.CompTape[]
, Forward extends string> =
  Pat extends []
    ? [Forward, S]
  : Pat extends [infer First extends rc.CompTape, ...infer _Rest extends rc.CompTape[]]
    ? _TapeEval<S, [First], Forward, []> extends [infer M extends string, infer Re extends string, infer _Tp extends rc.CompTape]
      ? AstaMatch<Re, Pat, M>
    : [Forward, S]
  : [Forward, S]


export type TapeEval<S extends string, Tape extends rc.CompTape[]> = _TapeEval<S, Tape, '', []>
type _TapeEval<
  String extends string
, Tape extends rc.CompTape
, Forward extends string
, Stack extends rc.CompTape[]> =
  Tape extends []
    ? Stack extends [ infer stackFirst extends rc.CompTape
                    , ...infer stackRest extends rc.CompTape[]]
      ? _TapeEval<String, stackFirst, Forward, stackRest>
    : [Forward, String, Tape]
  : Tape extends [ infer firstTape extends rc.CompTape
                 , ...infer restTape extends rc.CompTape[]]
    ? firstTape extends string
      ? str.StrSearchAll<String, firstTape, '^', Forward> extends [infer Matched extends string, infer Next extends string]
        ? _TapeEval<Next, restTape, Matched, Stack>
      : Unmatch
    : firstTape extends [ infer tapeSig extends ('?' | '*')
                        , infer tapeArg extends rc.CompTape]
      ? tapeSig extends '*'
        ? AstaMatch<String, [tapeArg], Forward> extends [infer nextForward extends string, infer NextString extends string]
          ? _TapeEval<NextString, restTape, nextForward, Stack>
        : never
      : tapeSig extends '?'
        ? _TapeEval<String, [tapeArg], Forward, [restTape, ...Stack]> extends [infer Matched extends string, infer NextString extends string, infer NextTape extends rc.CompTape]
          ? _TapeEval<NextString, NextTape, Matched, Stack>
        : _TapeEval<String, restTape, Forward, Stack>
      : never
    : firstTape extends [ 'or-group'
                        , ...infer tapeArg extends rc.CompTape[]]
      ? OrGroupMatch<String, tapeArg, Forward> extends [infer Matched extends string, infer NextString extends string, infer _NextTape extends rc.CompTape]
        ? _TapeEval<NextString, restTape, Matched, Stack>
      : Unmatch
    : firstTape extends ['group', infer tapeArg extends rc.CompTape]
      ? OrGroupMatch<String, [tapeArg], Forward> extends [infer Matched extends string, infer NextString extends string, infer _NextTape extends rc.CompTape]
        ? _TapeEval<NextString, restTape, Matched, Stack>
      : Unmatch
    : firstTape extends ['chara-class', infer tapeArg extends string[]]
      ? OrMatch<String, tapeArg, Forward, '!wildcard'> extends [infer Matched extends string, infer Next extends string]
        ? _TapeEval<Next, restTape, Matched, Stack>
      : Unmatch
    : firstTape extends infer wraped extends rc.CompTape[]
      ? _TapeEval<String, [...wraped], Forward, Stack> extends [infer Matched extends string, infer NextString extends string, infer NextTape extends rc.CompTape[]]
        ? _TapeEval<NextString, [...NextTape, ...restTape], Matched, Stack>
      : Unmatch
    : { error: `'firstTape' doens't match.`
      , dump: { firstTape: firstTape } }
  : never

export type * as regexEval from './regex-eval'
