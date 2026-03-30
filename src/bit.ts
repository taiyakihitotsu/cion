import type * as Peano from "./peano";

// CONSTANTS.
export type MAX = Peano.T16;
export type Zero = BitPadding<"0", MAX>;

// export const Pad8  = '00000000'
// export const Pad16 = `${Pad8}${Pad8}`
// export const CurPad = Pad8
export const Pad8: '00000000'  = '00000000'
export const Pad16: `${typeof Pad8}${typeof Pad8}` = `${Pad8}${Pad8}`
export type CurPadType = typeof Pad8
export const CurPad: CurPadType = Pad8

export type Min1 = '1111111111111111'

// -------------------------
// bit ops
export type BitOr<
  B
, C> =
  B extends `${infer BH}${infer BR}`
    ? C extends `${infer CH}${infer CR}`
      ? CH extends `1`
        ? `1${BitOr<BR, CR>}`
      : BH extends `1`
        ? `1${BitOr<BR, CR>}`
      : `0${BitOr<BR, CR>}`
    : ``
  : ``

export type BitAnd<
  B
, C> =
  B extends `${infer BH}${infer BR}`
    ? C extends `${infer CH}${infer CR}`
      ? CH extends `1`
        ? BH extends `1`
          ? `1${BitAnd<BR, CR>}`
        : `0${BitAnd<BR, CR>}`
      : `0${BitAnd<BR, CR>}`
    : ``
  : ``

export type BitXor<
  B
, C> =
  B extends `${infer BH}${infer BR}`
    ? C extends `${infer CH}${infer CR}`
      ? CH extends BH
        ? `0${BitXor<BR, CR>}`
      : `1${BitXor<BR, CR>}`
    : ``
  : ``

export type BitShiftLeftOne<
  B> =
  B extends `${infer H}`
    ? `${H}0` extends `${infer C}${infer D}`
      ? D
    : never
  : never

export type BitShiftLeft<
  B
, N> =
  N extends Peano.T0
    ? B
  : BitShiftLeft<BitShiftLeftOne<B>, Peano.dec<N>>

export type BitNot<
  B> =
  B extends `0`
    ? `1`
  : B extends `1`
    ? `0`
  : B extends `${infer H}${infer T}`
    ? `${BitNot<H>}${BitNot<T>}`
  : never

export type BitEq<
  B
, C> =
  B extends ""
    ? C extends ""
      ? true
    : false
  : B extends `${infer HB}${infer TB}`
    ? C extends `${infer HC}${infer TC}`
      ? C extends B
        ? B extends C
          ? BitEq<TB, TC>
        : false
      : false
    : false
  : false

export type BitLen<
  B
, count = Peano.T0> =
  B extends `${infer HB}${infer TB}`
    ? HB extends "0" | "1"
      ? BitLen<TB, Peano.inc<count>>
    : never
  : count

export type BitLenGthan<B, C> = Peano.gthan<BitLen<B>, BitLen<C>>;

export type _BitNeedFill<B, L> = Peano.min<BitLen<B>, L>;

export type BitPadding<
  B extends string
, P = Peano.T0
, F = "0"> =
  P extends Peano.T0
    ? B
  : F extends "0" | "1"
    ? BitPadding<`${F}${B}`, Peano.dec<P>, F>
  : never

export type BitUniform<
  B extends string
, C extends string> =
  BitLenGthan<
  B,
  C
> extends true
    ? [B, BitPadding<C, Peano.min<BitLen<B>, BitLen<C>>>]
  : [BitPadding<B, Peano.min<BitLen<C>, BitLen<B>>>, C]

export type BitCut<
  B
, P = Peano.T0> =
  P extends Peano.T0
    ? B
  : B extends `${infer H}${infer T}`
    ? BitCut<T, Peano.dec<P>>
  : never

// todo
export type BitIsZero<
  B extends string> =
  BitUniform<Zero, B> extends [
  infer Z,
  infer U,
]
    ? BitEq<Zero, U>
  : never

type BitFillError0 = 'BitFillError0'
export type BitFill<
  B extends string
, M = MAX
, tB extends string = BitPadding<B, M>> =
BitCut<tB, Peano.min<BitLen<tB>, M>>

// note : unsinged
export type _BitAdd<
  B
, C> =
  BitXor<B, C> extends infer _Xor
    ? BitAnd<B, C> extends infer _And
      ? BitShiftLeftOne<_And> extends string & infer _Carry extends string
        ? BitIsZero<
          BitUniform<_Carry, "0"> extends [infer _C extends string, infer _R]
            ? _C
            : never
        > extends true
          ? _Xor
        : _BitAdd<_Xor, _Carry>
      : 0
    : 1
  : 2

export type BitAdd<
  B extends string
, C extends string
, M = MAX> =
  BitFill<
  B,
  M
> extends infer _tB
    ? BitFill<C, M> extends infer _tC
      ? _BitAdd<_tB, _tC>
    : never
  : never

export type BitSub<
  B extends string
, C extends string
, M = MAX> =
BitAdd<
  BitFill<B, M>,
  BitAdd<BitNot<BitFill<C, M>>, BitFill<"1", M>>
>

export type BitGTE<
  B extends string
, C extends string> =
  BitSub<B,C> extends `${infer H}${infer _}`
    ? H extends '1'
      ? false
    : true
  : never

export type BitGT<
  B extends string
, C extends string> =
  BitGTE<B,C> extends true
    ? B extends C
      ? C extends B
        ? false
      : true
    : true
  : false

export type BitLT<
  B extends string
, C extends string> =
  BitGTE<B,C> extends true
    ? false
  : true

export type BitLTE<
  B extends string
, C extends string> =
  BitGT<B,C> extends true
    ? false
  : true

export type BitMul<
  B extends string
, C extends string
, M = MAX
, R extends string = BitFill<"0", M>
, tB extends string = BitFill<B, M>
, tC extends string = BitFill<C, M>
, N = Peano.dec<M>> =
  tB extends `${infer H}${infer T}`
    ? H extends "/"
      ? R
    : H extends "0"
      ? BitMul<B, C, M, R, `${T}/`, tC, Peano.dec<N>>
    : H extends "1"
      ? BitMul<
            B,
            C,
            M,
            BitAdd<R, BitShiftLeft<tC, N>>,
            `${T}/`,
            tC,
            Peano.dec<N>
          >
    : never
  : never

export type _BitShiftRight<
  B extends string
, M = MAX
, N = Peano.T1
, D = Peano.min<M, N>
, R extends string = ""> =
  D extends Peano.T0
    ? R extends ""
      ? B
    : R
  : B extends `${infer H}${infer T}`
    ? _BitShiftRight<`${T}`, M, N, Peano.dec<D>, `${R}${H}`>
  : R

// export type BitShiftRight
// export type UnsignedBitShiftRight

// note : 
// I think peano number 2 should be written from [[null]] to [null,null].
// If so, implementing div with comparing and add is good performance than minus or some hack.
// Because we can use concat in this case.

export type BitRevSign<
  S extends string> =
BitMul<S, Min1>

export type _BitDiv<
  B extends string
, C extends string
, Ret extends string = "00000000"> =
  BitLT<B,C> extends true
    ? Ret
  : _BitDiv<BitSub<B,C>, C, BitAdd<Ret, "00000001">>

// todo
export type Nil  = [`prim`, `nil`]
export const nil: Nil = [`prim`, `nil`] 

export type BitDiv<
  B extends string
, C extends string> =
  BitIsZero<C> extends true
    ? Nil
  : BitFill<B,MAX> extends `${infer bh}${infer br}`
    ? BitFill<C,MAX> extends `${infer ch}${infer cr}`
      ? _BitDiv<bh extends '1' ? BitRevSign<`${bh}${br}`> : `${bh}${br}`,
                ch extends '1' ? BitRevSign<`${ch}${cr}`> : `${ch}${cr}`> extends infer dd
        ? '0' | '1' extends bh | ch
          ? BitFill<BitRevSign<dd extends string ? dd : never>, MAX>
        : BitFill<dd extends string ? dd : never, MAX>
      : never
    : never
  : never

export type BitAbs<B extends string> = BitLT<B, Zero> extends true ? BitMul<B, '1111111111111111'> : B

export type _BitMod<
  B extends string
, C extends string
, Ret extends string = B> =
  BitLT<Ret,C> extends true
    ? Ret
  : _BitMod<B,C,BitSub<Ret,C>>

export type BitMod<
  B extends string
, C extends string> =
  BitIsZero<C> extends true
    ? Nil
  : [BitLT<B, Zero>, BitLT<C, Zero>] extends [true, false]
    ? _BitMod<BitSub<C, _BitMod<BitRevSign<B>, C>>, C>
  : [BitLT<B, Zero>, BitLT<C, Zero>] extends [false, true]
    ? BitMul<_BitMod<B, BitRevSign<C>>, '1111111111111111'>
  : BitFill<_BitMod<BitAbs<B>, BitAbs<C>>, MAX>

export type BitDec<B extends string> = BitSub<B, "0000000000000001">
export type BitInc<B extends string> = BitAdd<B, "0000000000000001">

export * as Bit from './bit'
