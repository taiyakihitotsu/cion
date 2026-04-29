import type * as Decimal from '../decimal/index.js'
import type * as ratio from '../ratio/index.js'
import type { NilLiteral } from '../sexprtypes.js'

/** Wrap with corresponding bracket-set. */
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
/**
Converts internal AST to Lisp's decimal number literal, specifically for numeric AST nodes.

This type transforms the internal AST representation of a number (expressed as a pair 
  of 16-bit binary strings representing a ratio) into a human-readable Lisp decimal 
  or fractional string literal.

A tuple matching `['prim', [NumeratorBits, DenominatorBits]]`.

 Logic Flow:
 1. Checks if the denominator is valid (not `'nil'`).
 2. Applies `Normalize` reduction to simplify the ratio.
 3. If the simplified denominator is '1', it returns the numerator as a decimal string.
 4. If the denominator is > 1, it returns a fractional string format: "numerator/denominator".
 5. Handles signed values by checking the leading bit of the numerator bitstring.

```typescript
type Literal_0_0 = SNumberString<['prim', ['0000000000000000', '0000000000000000']]> //=> 'nil'
type Literal_0_1 = SNumberString<['prim', ['0000000000000000', '0000000000000001']]> //=> '0'
type Literal_1_1 = SNumberString<['prim', ['0000000000000001', '0000000000000001']]> //=> '1'
type Literal_8_2 = SNumberString<['prim', ['0000000000001000', '0000000000000010']]> //=> '4'
type Literal_8_3 = SNumberString<['prim', ['0000000000001000', '0000000000000011']]> //=> '8/3'
type Literal_m1_m1 = SNumberString<['prim', ['1111111111111111', '1111111111111111']]> //=> '1'
type Literal_m8_m2 = SNumberString<['prim', ['1111111111111000', '1111111111111110']]> //=> '4'
type Literal_m8_m3 = SNumberString<['prim', ['1111111111111000', '1111111111111101']]> //=> '8/3'
type Literal_m8_3 = SNumberString<['prim', ['1111111111111000', '0000000000000011']]> //=> '-8/3'
type Literal_8_m3 = SNumberString<['prim', ['0000000000001000', '1111111111111101']]> //=> '-8/3'
```
*/
export type SNumberString<
  S extends ['prim', [string, string]]> =
  ratio.ForceNat<S[1]> extends infer D extends string
    ? D extends 'nil'
      ? 'nil'
    : ratio.Normalize<S[1]> extends [ infer f extends string, infer s extends string ]
      ? s extends '0000000000000001' // [todo] import
        ? Decimal.BitToDecimal<f>
      : `${Decimal.BitToDecimal<f>}/${Decimal.BitToDecimal<s>}`
    : NilLiteral
  : never

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

/**
Core part.
Internal AST to S-expression for ALL expressions.

Recursively converts an internal AST back into a human-readable S-expression string.
This process represents the "Unparse" (or stringify) phase and does not evaluate the expressions.

Dont not evaluate expressions.

```typescript
type c = SUnparse<['prim', true]> //=> 'true'
type b = SUnparse<['prim', 'string']> //=> "'string'"
type a = SUnparse<['if', ['prim', true], ['prim', '0000000000000001']]> //=> '(if true 1)'
```
*/
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
      ? SNumberString<H>
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

export type SUnparse<AST> = _Unparse<AST> extends infer r ? r extends '' ? 'nil' : r : never
