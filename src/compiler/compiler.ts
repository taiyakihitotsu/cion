import type { Symbolizer } from './symbolizer.js'

/**
Internal helper to identify string literals within the token stream.
Returns `true` if the token is enclosed in double quotes.
*/
type IsStrValue<S extends string> = S extends `"${infer _}"` ? true : false

/**
Compiles a sequence of tokens into an internal AST (Abstract Syntax Tree).

It transforms raw string tokens (numbers, fractions, symbols, keys, and literals) 
into structured tuples like ['prim', ...], ['sym', ...], or ['map', ...].

Features:
- Converts decimal strings and fractions into 16-bit binary representations.
- Handles nested structures including Lists (), Vectors [], and Maps {}.
- Preserves Lisp-style keywords (:key) and symbols.
*/
export type SCompiler<
  Parsed extends Array<unknown>
, Current extends Array<unknown> = []
, Stack extends Array<Array<unknown>> = []
, StrStack extends string = ""
, IsLetVec extends boolean = false> =
  Parsed extends []
    ? Current
  : Parsed extends [infer H extends string, ...infer R extends string[]]
    ? R extends []
      ? IsStrValue<H> extends true
        ? [`prim`, H]
      : H extends "}"
        ? ['map', Current]
      : Current extends []
        ? Symbolizer<H>
      : Current
    : H extends ')' | ']'
      ? SCompiler<
            R
          , Stack extends unknown[] // the case of empty vector.
            ? [...Stack[0], Current extends ['vec', never] ? ['vec'] : Current]
            : never
          , Stack extends [infer _, ...infer R extends unknown[][]] ? R : never>
    : H extends '}'
      ? SCompiler<R, Stack extends Array<unknown> ? [...Stack[0], ['map', Current]] : never, Stack extends [infer _, ...infer R extends unknown[][]] ? R : never>
    : H extends '('
      ? SCompiler<R, [], [Current, ...Stack]>
    : H extends '['
      ? SCompiler<R, IsLetVec extends true ? [] : ['vec'], [Current, ...Stack], StrStack>
    : H extends '{'
      ? SCompiler<R, [], [Current, ...Stack]>
    : H extends `:${infer _}`
      ? SCompiler<R, [...Current, [`key`, H]], Stack>
    : SCompiler<R, [...Current, Symbolizer<H>], Stack, StrStack, H extends 'let' | 'fn' ? true : false>
  : never
