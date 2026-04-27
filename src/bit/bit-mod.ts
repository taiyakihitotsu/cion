import type { BitLT } from './bit-lt.js'
import type { BitSub } from './bit-sub.js'
import type { BitIsZero } from './bit-is-zero.js'
import type { MAX } from './const.js'
import type { BitRevSign } from './bit-rev-sign.js'
import type { BitFill } from './bit-fill.js'
import type { TNil } from '../sexprtypes.js'

/**
Internal helper: Long division to find the remainder.
This tracks the 'running remainder' bit by bit.
*/
type _BitMod<
  Dividend extends string,
  Divisor extends string,
  Remainder extends string = ""> =
  Dividend extends `${infer Head}${infer Tail}`
    ? `${Remainder}${Head}` extends infer NextRemainder extends string
      ? BitLT<NextRemainder, Divisor> extends true
        // Cannot subtract: remainder just accumulates the next bit
	? _BitMod<Tail, Divisor, NextRemainder>
      // Can subtract: subtract divisor from current running remainder
      : _BitMod<Tail, Divisor, BitSub<NextRemainder, Divisor>>
    : never
  : BitFill<Remainder, MAX>

/**
Internal long division for Clojure-style 'mod' (Floored Division).

https://clojuredocs.org/clojure.core/mod
*/
export type BitMod<
  B extends string
, C extends string> =
  BitIsZero<C> extends true
    ? TNil
  : [BitFill<B, MAX>, BitFill<C, MAX>] extends [infer FB extends string, infer FC extends string] 
    ? [FB, FC] extends [`${infer bh}${string}`, `${infer ch}${string}`]
      ? _BitMod<bh extends '1' ? BitRevSign<FB> : FB, ch extends '1' ? BitRevSign<FC> : FC> extends infer AbsMod
        ? AbsMod extends string
          ? BitIsZero<AbsMod> extends true
            ? BitFill<"0", MAX>
          : [bh, ch] extends ['0', '0']
            ? BitFill<AbsMod, MAX>
	  : [bh, ch] extends ['1', '0']
            ? BitSub<FC, BitFill<AbsMod, MAX>>
	  : [bh, ch] extends ['0', '1']
            ? BitRevSign<BitSub<BitRevSign<FC>, BitFill<AbsMod, MAX>>>
          : BitRevSign<BitFill<AbsMod, MAX>>
        : never
      : never
    : never
  : never
