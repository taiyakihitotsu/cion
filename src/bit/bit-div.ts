import type { MAX } from './const.js'
import type { BitLT } from './bit-lt.js'
import type { BitSub } from './bit-sub.js'
import type { BitAdd } from './bit-add.js'
import type { BitIsZero } from './bit-is-zero.js'
import type { BitFill } from './bit-fill.js'
import type { BitRevSign } from './bit-rev-sign.js'
import type { TNil } from '../sexprtypes.js'

export type _BitDiv<
  B extends string
, C extends string
, Ret extends string = "00000000"> =
  BitLT<B,C> extends true
    ? Ret
  : _BitDiv<BitSub<B,C>, C, BitAdd<Ret, "00000001">>

export type BitDiv<
  B extends string
, C extends string> =
  BitIsZero<C> extends true
    ? TNil
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
