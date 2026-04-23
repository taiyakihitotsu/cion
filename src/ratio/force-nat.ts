import type { IntBitString, Ratio, DivByZero } from './const.js'
import type { BitDiv } from '../bit/index.js'
import type { DecimalToBit } from '../decimal/index.js'

/**
Coerces a ratio-tuple into a bit-string.
Force means that this returns the `Trunc` result if the ratio-tuple cannot be divided evenly.

```typescript
type Expected_5 = ForceNat<["0000000000001010", "0000000000000010"]> //=> "0000000000000101"
type Expected_force3 = ForceNat<["0000000000001010", "0000000000000011"]> //=> "0000000000000011"
```
*/
export type ForceNat<
  Z extends IntBitString | Ratio> =
  Z extends [ infer xc extends IntBitString
            , infer xm extends IntBitString]
    ? xm extends DecimalToBit<'0'>
      ? DivByZero
    : BitDiv<xc, xm>
  : Z extends IntBitString
    ? Z
  : never


