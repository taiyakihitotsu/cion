import type { Ratio, IntBitString } from './const.js'
import type { BitEq, BitOne } from '../bit/index.js'
import type { BitToDecimal } from '../decimal/index.js'

/**
Returns a ratio-string translated from a ratio-tuple argument.

NOTE: Not automatically scales.

```typescript
type Expected_0 = ratio.RatioStr<['0000000000000000', '0000000000000001']> //=> '0'
type Expected_neg1 = ratio.RatioStr<['1111111111111111', '0000000000000001']> //=> '-1'
type Expected_not_scale = ratio.RatioStr<['0000000000000100', '0000000000000010']> //=> '4/2'
```
*/
export type RatioStr<
  XY extends Ratio> =
  XY extends [infer x extends IntBitString
             , infer y extends IntBitString]
    ? BitEq<y, BitOne> extends true
      ? `${BitToDecimal<x>}`
    : `${BitToDecimal<x>}/${BitToDecimal<y>}`
  : never
