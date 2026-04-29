import type { IntBitString, Nat, Ratio, RatioNumber } from './const.js'
import type { BitGT, BitLT, BitGTE, BitLTE, BitEq } from '../bit/index.js'
import type { Scaling } from './scaling.ts'

export type BitR<
  xc extends IntBitString
, yc extends IntBitString
, r extends '>' | '<' | '>=' | '<=' | '='> =
  r extends '>'
    ? BitGT<xc, yc>
  : r extends '<'
    ? BitLT<xc, yc>
  : r extends '>='
    ? BitGTE<xc, yc>
  : r extends '<='
    ? BitLTE<xc, yc>
  : r extends '='
    ? BitEq<xc, yc>
  : never

type Test_Int_GT = BitGT<'0000000000000011', '0000000000000010'>

/**
Compares two RatioNumbers based on the specified relation.

If both are Ratios, they are scaled to a common denominator before comparing their numerators.

`X` and `Y` must be 16-digit-bit-strings.
*/
export type Relation<
  X extends RatioNumber
, Y extends RatioNumber
, r extends '>' | '<' | '>=' | '<=' | '='> =
  [X, Y] extends [infer x extends Nat, infer y extends Nat]
    ? BitR<x,y,r>
  : [X, Y] extends [infer x extends Ratio, infer y extends Ratio]
    ? Scaling<x, y> extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                        , [ infer yc extends IntBitString
                            , infer ym extends IntBitString ] ]
      ? BitR<xc,yc,r>
    : never
  : never
