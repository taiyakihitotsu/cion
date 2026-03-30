import type { Bit } from './bit'
import type { DecimalToBit } from './decimal'

/**
This defines utility for tuples.
Usable for `regex-compiler.ts`.

Every Numbers are implicitly regarded as `BitString`.
So, this is not `number` but `string`.

Some types are redundant but I've leaved them just in cases.
*/

/**
`BitDrop<BitString, Tuple>`

https://clojuredocs.org/clojure.core/drop
*/
export type BitDrop<N extends string, V extends unknown[]> = _BitDrop<N, V>

type _BitDrop<
  N extends string
, V extends unknown[]> =
  V extends []
    ? V
  : true extends Bit.BitGTE<"0", N>
    ? V
  : V extends [infer _, ...infer T extends unknown[]]
    ? _BitDrop<Bit.BitSub<N, "1">, T>
  : 0

/**
`BitTake<BitString, Tuple>`

https://clojuredocs.org/clojure.core/take
*/
export type BitTake<N extends string, V extends unknown[]> = _BitTake<N, V>

type _BitTake<
  N extends string
, V extends unknown[]
, R extends unknown[] = []> =
  V extends []
    ? R
  : Bit.BitGTE<"0", N> extends true
    ? R
  : V extends [infer F, ...infer T]
    ? _BitTake<Bit.BitSub<N, "1">, T, [...R, F]>
  : []

export type BitInter<
  V extends unknown[]
, N extends string
, M extends string> =
BitDrop<N, BitTake<Bit.BitInc<M>, V>>

export type BitRepeat<
  N extends string
, V> =
_BitRepeat<N, V>

type _BitRepeat<
  N extends string
, V
, R extends V[] = []> =
  Bit.BitGTE<'0000000000000000',N> extends true
    ? R
  : _BitRepeat<Bit.BitDec<N>, V, [...R, V]>

export type recRepeat<
  N extends string
, V> =
{ r: _recRepeat<N, V> }

type _recRepeat<
  N extends string
, V
, R extends V[] = []> =
  Bit.BitGTE<'0000000000000000',N> extends true
    ? { r: R }
  : { r: _recRepeat<Bit.BitDec<N>, V, [...R, V]> }

export type Last<
  V extends unknown[]> =
  DecimalToBit<`${V['length']}`> extends infer R extends string
    ? BitDrop<Bit.BitSub<R, "1">, V> extends [infer E]
      ? E
    : never
  : never

export type * as VecUtil from './vecutil'
