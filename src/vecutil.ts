import type { Bit } from './bit'

type _Drop<
  N extends string
, V extends unknown[]
, R extends unknown[] = []> =
  V extends []
    ? R
  : V extends [infer _, ...infer T]
    ? Bit.BitGTE<"0", N> extends true
      ? V
    : _Drop<Bit.BitSub<N, "1">, T>
  : []

export type Drop<N extends string, V extends unknown[]> = _Drop<N, V>

type _Take<
  N extends string
, V extends unknown[]
, R extends unknown[] = []> =
  V extends []
    ? R
  : Bit.BitGTE<"0", N> extends true
    ? R
  : V extends [infer F, ...infer T]
    ? _Take<Bit.BitSub<N, "1">, T, [...R, F]>
  : []

type Take<N extends string, V extends unknown[]> = _Take<N, V>

export type Inter<
  V extends unknown[]
, N extends string
, M extends string> =
Drop<N, Take<Bit.BitInc<M>, V>>

type _Repeat<
  N extends string
, V
, R extends V[] = []> =
  Bit.BitGTE<'0000000000000000',N> extends true
    ? R
  : _Repeat<Bit.BitDec<N>, V, [...R, V]>

export type Repeat<
  N extends string
, V> =
_Repeat<N, V>

type _Last<
  V extends unknown[]
, R extends unknown[] = []> =
  V extends [infer F, ...infer T]
    ? T extends []
      ? [...R, F]
    : _Last<T, [...R, F]>
  : never

export type Last<V extends unknown[]> = _Last<V>
  
export type * as VecUtil from './vecutil'
