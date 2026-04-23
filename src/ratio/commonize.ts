import type { Ratio, IntBitString, RatioZero } from './const.js'
import type { BitIsZero, BitLT, BitDiv, BitRevSign, BitAbs, BitZero } from '../bit/index.js'
import type { GCD } from './gcd.js'

/**
Simplifies a ratio by reducing it to its lowest terms and normalizing the sign.
- Uses GCD for reduction.
- Ensures the denominator is positive by adjusting signs if necessary.
*/
export type Commonize<
  S extends Ratio> =
  S extends [infer c extends IntBitString, infer d extends IntBitString]
    ? BitIsZero<c> extends true
      ? RatioZero
    : GCD<c,d> extends infer bc extends IntBitString
      ? [BitDiv<c, bc>, BitDiv<d, bc>, BitLT<c, BitZero>, BitLT<d, BitZero>] extends [infer cr extends IntBitString, infer dr extends IntBitString, infer cs extends boolean, infer ds extends boolean]
        ? cs & ds extends never
          ? ds extends true
            ? [BitRevSign<cr>, BitRevSign<dr>]
          : [cr, dr]
        : [BitAbs<cr>, BitAbs<dr>]
      : never
    : never
  : never
