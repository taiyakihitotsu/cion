import type { IntBitString, Ratio } from './const.js'
import type { BitDiv, BitLT, BitEq, BitDec } from '../bit/index.js'
import type { GCD } from './gcd.js'

export type Floor<
  Z extends IntBitString | Ratio> =
  Z extends IntBitString
    ? Z
  : Z extends Ratio & [infer X extends IntBitString, infer Y extends IntBitString]
    ? [GCD<X,Y>, BitDiv<X,Y>] extends [infer tGCD extends IntBitString, infer tDiv extends IntBitString]
      ? [BitEq<tGCD, Y>, BitLT<X, '0'> & BitLT<Y, '0'>] extends [false, never]
        ? BitDec<tDiv>
      : tDiv
    : never
  : never
