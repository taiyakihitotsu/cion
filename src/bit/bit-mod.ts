import type { BitLT } from './bit-lt.js'
import type { BitSub } from './bit-sub.js'
import type { BitIsZero } from './bit-is-zero.js'
import type { Neg1, BitZero, MAX } from './const.js'
import type { BitAbs } from './bit-abs.js'
import type { BitMul } from './bit-mul.js'
import type { BitRevSign } from './bit-rev-sign.js'
import type { BitFill } from './bit-fill.js'
import type { TNil } from '../sexprtypes.js'

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
    ? TNil
  : [BitLT<B, BitZero>, BitLT<C, BitZero>] extends [true, false]
    ? _BitMod<BitSub<C, _BitMod<BitRevSign<B>, C>>, C>
  : [BitLT<B, BitZero>, BitLT<C, BitZero>] extends [false, true]
    ? BitMul<_BitMod<B, BitRevSign<C>>, Neg1>
  : BitFill<_BitMod<BitAbs<B>, BitAbs<C>>, MAX>
