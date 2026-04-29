import type { MAX } from './const.js'
import type { BitLT } from './bit-lt.js'
import type { BitSub } from './bit-sub.js'
import type { BitIsZero } from './bit-is-zero.js'
import type { BitFill } from './bit-fill.js'
import type { BitRevSign } from './bit-rev-sign.js'
import type { TNil } from '../sexprtypes.js'

/**
Perform bitwise division using the long division algorithm.
*/
type _BitDiv<
  Dividend extends string,
  Divisor extends string,
  Quotient extends string = "",
  Remainder extends string = ""> =
  Dividend extends `${infer Head}${infer Tail}`
    ? `${Remainder}${Head}` extends infer NextRemainder extends string
      ? BitLT<NextRemainder, Divisor> extends true
	// Cannot subtract: quotient bit is 0
	? _BitDiv<Tail, Divisor, `${Quotient}0`, NextRemainder>
	// Can subtract: quotient bit is 1, subtract divisor from remainder
      : _BitDiv<Tail, Divisor, `${Quotient}1`, BitSub<NextRemainder, Divisor>>
    : never
  : BitFill<Quotient, MAX>;

export type BitDiv<
  B extends string,
  C extends string> = 
  BitIsZero<C> extends true
    ? TNil
  : [BitFill<B, MAX>, BitFill<C, MAX>] extends [infer FB extends string, infer FC extends string]
    ? [FB, FC] extends [`${infer bh}${string}`, `${infer ch}${string}`]
      ? _BitDiv<
	  bh extends '1' ? BitRevSign<FB> : FB,
	  ch extends '1' ? BitRevSign<FC> : FC
	> extends infer Res extends string
	? bh extends ch
	  ? Res
	: BitRevSign<Res>
      : never
    : never
  : never;
