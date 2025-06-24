import type * as Bit from './bit'
import type * as Decimal from './decimal'
import type * as ratio from './ratio'

// -------------------------------
// -- Compiler
// -------------------------------

export type SPad<S extends string> = S extends ` ${infer SS}` ? SS  : ` ${S}`

// NOTE (A)
//
// Don't delete this for an info.
// See the below (A) note.
//
// export type LegacySParser<Sexpr> =
//   // -- ()
//   Sexpr extends ` (${infer U}`
//     ? ['(', ...LegacySParser<` ${U}`>]
//   : Sexpr extends ` ${infer V} ${infer W}`
//     ? [...LegacySParser<` ${V}`>, ...LegacySParser<` ${W}`>]
//   : Sexpr extends ` ${infer C})`
//     ? [...LegacySParser<` ${C}`>, ')']
//   // -- []
//   : Sexpr extends ` [${infer U}`
//     ? ['[', ...LegacySParser<` ${U}`>]
//   : Sexpr extends ` ${infer V} ${infer W}`
//     ? [...LegacySParser<` ${V}`>, ...LegacySParser<` ${W}`>]
//   : Sexpr extends ` ${infer C}]`
//     ? [...LegacySParser<` ${C}`>, ']']
//   // -- {}
//   : Sexpr extends ` {${infer U}`
//     ? ['{', ...LegacySParser<` ${U}`>]
//   : Sexpr extends ` ${infer V} ${infer W}`
//     ? [...LegacySParser<` ${V}`>, ...LegacySParser<` ${W}`>]
//   : Sexpr extends ` ${infer C}}`
//     ? [...LegacySParser<` ${C}`>, '}']
//   // -- ""
//   : Sexpr extends ` "${infer U}`
//     ? [`"`, ...LegacySParser<` ${U}`>]
//   : Sexpr extends ` ${infer V} ${infer W}`
//     ? [...LegacySParser<` ${V}`>, ...LegacySParser<` ${W}`>]
//   : Sexpr extends ` ${infer C}"`
//     ? [...LegacySParser<` ${C}`>, `"`]
//   // -- _, as default.
//   : Sexpr extends ` ${infer CC}`
//     ? [CC]
//     : []

type _rec<
  T> =
  T extends {r: never}
    ? never
  : T extends {r: {r: {r: {r: {r: {r: {r: {r: infer U}}}}}}}}
    ? { r: _rec<U> }
  : T extends {r: {r: {r: {r: infer U}}}}
    ? { r: _rec<U> }
  : T extends {r: {r: infer U}}
    ? { r: _rec<U> }
  : T extends {r: infer U}
    ? U
  : T

export type Rec<
  T> =
  T extends {r: unknown}
    ? Rec<_rec<T>>
  : T

export type ReadString<
  S extends string
, ident extends `"` | `'`> =
  S extends `${ident}${infer P}${ident}${infer R}`
    ? [`${ident}${P}${ident}`, ` ${R}`]
  : never

export type recp<
  Sexpr
, R extends string[] = []> =
  Sexpr extends `  ${infer U}` | ` \n${infer U}`
    ? { r: recp<` ${U}`, R> }
  : Sexpr extends ` (${infer U}`
    ? { r: recp<` ${U}`, [...R, '(']> }
  : Sexpr extends ` {${infer U}`
    ? { r: recp<` ${U}`, [...R, '{']> }
  : Sexpr extends ` [${infer U}`
    ? { r: recp<` ${U}`, [...R, '[']> }
  : Sexpr extends ` "${infer U}`
    ? { r: recp<ReadString<`"${U}`, `"`>[1], [...R, ReadString<`"${U}`, `"`>[0]]> }
  : Sexpr extends ` '${infer U}`
    ? { r: recp<ReadString<`'${U}`, `'`>[1], [...R, ReadString<`'${U}`, `'`>[0]]> }
  : Sexpr extends ` ${infer fU} ${infer Next}`
    ? fU extends `${infer ffU}}`
      ? ffU extends ''
        ? { r: recp<` ${Next}`, [...R, '}']> }
      : { r: recp<` ${ffU} } ${Next}`, R> }
    : fU extends `${infer ffU}]`
      ? ffU extends ''
        ? { r: recp<` ${Next}`, [...R, ']']> }
      : { r: recp<` ${ffU} ] ${Next}`, R> }
    : fU extends `${infer ffU})`
      ? ffU extends ''
        ? { r: recp<` ${Next}`, [...R, ')']> }
      : { r: recp<` ${ffU} ) ${Next}`, R> }
    : fU extends `${infer ffU}"`
      ? { r: recp<` ${Next}`, [...R, ffU, '"']> }
    : { r: recp<` ${Next}`, [...R, fU]> }
  : Sexpr extends ` ${infer U})`
    ? { r: recp<` ${U} ) `, R> }
  : Sexpr extends ` ${infer U}}`
    ? { r: recp<` ${U} } `, R> }
  : Sexpr extends ` ${infer U}]`
    ? { r: recp<` ${U} ] `, R> }
  : Sexpr extends ` ${infer U}`
    ? { r: [...R, ...(U extends '' ? [] : [U])] }
  : { r: R }


export type SParser<Sexpr extends string> = Rec<recp<Sexpr>>

type NumUnion = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'

export type SIsNum<
  S
, Top extends boolean = true> =
  S extends `${infer H}${infer R}`
    ? H extends '-'
      ? Top extends true
        ? SIsNum<R, false> extends true
          ? true
        : false
      : false
    : H extends '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'
      ? R extends ''
        ? true
      : SIsNum<R, false>
    : false
  : false

type ReadRationalFail = []
// [note]
//   It's easy to write with `SIsNum` to get rid of `->` or so from the numbers.
export type ReadRational<
  S extends string
, Ret extends string[] = []
, SS extends string = ''> =
  S extends ''
    ? SS extends ''
      ? ReadRationalFail
    : SIsNum<SS> extends true
      ? [...Ret, SS]
    : ReadRationalFail
  : S extends `${infer H}${infer R}`
    ? H extends NumUnion | '-'
      ? ReadRational<R, Ret, `${SS}${H}`>
    : H extends '/'
      ? SIsNum<SS> extends true
        ? ReadRational<R, [...Ret, SS], ''>
      : ReadRationalFail
    : ReadRationalFail
  : never

export type SSymlator<
  MSym> =
  MSym extends `${infer H}${infer R}`
    ? H extends "'" | '"'
      ? [`prim`, MSym]
    : ReadRational<MSym> extends [`${infer F}${infer Fs}`, ...infer rD extends string[]]
      ? rD extends [`${infer _D}${infer _Ds}`]
        ? F extends '-'
          ? ['prim', [Bit.BitRevSign<Decimal.DecimalToBit<Fs>>, Decimal.DecimalToBit<`${_D}${_Ds}`>]]
        : ['prim', [Decimal.DecimalToBit<`${F}${Fs}`>, Decimal.DecimalToBit<`${_D}${_Ds}`>]]
      : F extends '-'
        ? ['prim', Bit.BitRevSign<Decimal.DecimalToBit<Fs>>]
      : ['prim', Decimal.DecimalToBit<`${F}${Fs}`>]
    : MSym extends 'if' | 'let' | 'fn'
      ? MSym
    : MSym extends 'true'
      ? [`prim`, true]
    : MSym extends 'false'
      ? [`prim`, false]
    : MSym extends 'nil'
      ? [`prim`, 'nil']
    : [`sym`, MSym]
  : never

type IsStrValue<S extends string> = S extends `"${infer _}"` ? true : false

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
        ? SSymlator<H>
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
    : SCompiler<R, [...Current, SSymlator<H>], Stack, StrStack, H extends 'let' | 'fn' ? true : false>
  : never

type CloseBracket<
  S extends string
, B extends string> =
  B extends 'map'
    ? `{${S}}`
  : B extends 'vec'
    ? `[${S}]`
  : B extends 'list'
    ? `(${S})`
  : S

export type SEncoder<
  V extends unknown[]
, Bracket extends 'map' | 'vec' | 'list' | 'unroll' = 'unroll'> =
  V extends ['sym', infer U extends string]
    ? `${U}`
  : V extends ['prim', infer U extends string | number | boolean]
    ? U extends string
      ? `'${U}'`
    : `${U}`
  : V extends ['map', infer U extends unknown[]]
    ? `{${SEncoder<U>}}`
  : V extends ['vec', ...infer U extends unknown[]]
    ? `[${SEncoder<U>}]`
  : V extends ['if'
		 , infer  B extends unknown[]
		 , infer TP extends unknown[]
		 , infer FP extends unknown[]]
    ? `(if ${SEncoder<B, 'list'>} ${SEncoder<TP, 'list'>} ${SEncoder<FP, 'list'>})`
  : V extends ['if'
		 , infer  B extends unknown[]
		 , infer TP extends unknown[]]
    ? `(if ${SEncoder<B, 'list'>} ${SEncoder<TP, 'list'>})`
  : V extends ['let'
		, infer U extends unknown[]
		, infer S extends unknown[]]
    ? `(let ${SEncoder<U, 'vec'>} ${SEncoder<S, 'list'>})`
  : V extends ['key', infer U extends string]
    ? `${U}`
  : V extends [infer U extends unknown[], ...infer R extends unknown[][]]
    ? R extends []
      ? CloseBracket<`${SEncoder<U>}`, Bracket>
    : CloseBracket<`${SEncoder<U>} ${SEncoder<R>}`, Bracket>
  : ''

export type _Unparse<
  AST
, Type extends 'vec' | 'list' | 'map' | 'atom' = 'list'> =
  AST extends infer H
    ? H extends ['prim' | 'sym' | 'key', infer r0 extends boolean | string]
      ? r0 extends string
        ? Decimal.IsBitExpr<r0> extends true
          ? Decimal.BitToDecimal<r0>
        : `${r0}`
      : `${r0}`
    : H extends ['prim', [string, string]]
      ? ratio.ForceNat<H[1]> extends infer D extends string
        ? D extends 'nil'
          ? 'nil'
        : ratio.LCD<H[1]> extends [ infer f extends string
                                  , infer s extends string]
          ? s extends '0000000000000001'
            ? Decimal.BitToDecimal<f>
          : `${Decimal.BitToDecimal<f>}/${Decimal.BitToDecimal<s>}`
        : 'nil'
      : 'never2'
    : H extends ['vec', ...infer r]
      ? r extends []
        ? '[]'
      : _Unparse<r, 'vec'>
    : H extends ['map', infer r]
      ? _Unparse<r, 'map'>
    : H extends ['fn', infer r0, infer r1]
      ? CloseBracket<`fn ${(_Unparse<r0, 'vec'> extends infer s ? s extends string ? s : '' : '')} ${(_Unparse<r1> extends infer s ? s extends string ? s : '' : '')}`, 'list'>
    : H extends ['let', infer r0, infer r1]
      ? CloseBracket<`let ${(_Unparse<r0, 'vec'> extends infer s ? s extends string ? s : '' : '')} ${(_Unparse<r1> extends infer s ? s extends string ? s : '' : '')}`, 'list'>
    : H extends ['if', infer r0, infer r1, ...infer r2]
      ? CloseBracket<`if ${(_Unparse<r0> extends infer s ? s extends string ? s : '' : '')} ${(_Unparse<r1> extends infer s ? s extends string ? s : '' : '')}${r2 extends [] ? '' : ' '}${(_Unparse<r2, 'atom'> extends infer s ? s extends string ? s : '' : '')}`, Type>
    : H extends [infer H extends unknown[], ...infer T]
      ? CloseBracket<`${(_Unparse<H> extends infer s ? s extends string ? s : '' : '')}${T extends [] ? '' : ' '}${(_Unparse<T, 'atom'> extends infer s ? s extends string ? s : '' : '')}`, Type>
    : H extends []
      ? ''
    : { ast: AST
      , error: 'InnerUnparseError' }
  : never

export type Unparse<AST> = _Unparse<AST> extends infer r ? r extends '' ? 'nil' : r : never
