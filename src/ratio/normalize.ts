import type { IntBitString, Ratio, RatioZero } from './const.js'
import type { BitIsZero, BitLT, BitDiv, BitAbs, BitRevSign, BitZero } from '../bit/index.js'
import type { GCD } from './gcd.js'
import type { NilLiteral } from '../../src/sexprtypes.js'

/**
Reduces a ratio to its simplest form using the Greatest Common Measurer (GCD).
It also normalizes the sign, ensuring the denominator remains positive.

```typescript
type A = Normalize<["1111111111110100", "1111111111111110"]> //=> ["0000000000000110", "0000000000000001"]
```
*/
export type Normalize<
  Z extends IntBitString | Ratio
, Error = NilLiteral> =
  Z extends IntBitString
    ? Z
  : Z extends [ infer xc extends IntBitString
              , infer xm extends IntBitString]
    ? BitIsZero<xc> extends true
      ? RatioZero
    : BitIsZero<xm> extends true
      ? Error
    : GCD<xc, xm> extends infer gcd extends IntBitString
      ? [ BitLT<xc, BitZero>
        , BitLT<xm, BitZero> ] extends [true, true]
        ? [BitDiv<BitAbs<xc>, gcd>, BitDiv<BitAbs<xm>, gcd>]
      : [ BitLT<xc, BitZero>
        , BitLT<xm, BitZero> ] extends [false, true]
        ? [BitDiv<BitRevSign<xc>, gcd>, BitDiv<BitAbs<xm>, gcd>]
      : [BitDiv<xc, gcd>, BitDiv<xm, gcd>]
    : Error
  : never
