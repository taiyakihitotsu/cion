import type { IntBitString, Ratio, DivByZero } from './const.js'
import type { DecimalToBit } from '../decimal/index.js'

/**
Ensures the input is returned as a ratio tuple.
- If the input is a bit-string, it returns a ratio with a denominator of 1.
- If the input is already a ratio tuple, it returns it as-is (validates denominator).

```typescript
type Rational = ForceRatio<"0000000000000011"> //=> ["0000000000000011", "0000000000000001"]
```
*/
export type ForceRatio<
  Z extends IntBitString | Ratio> =
  Z extends IntBitString
    ? [Z, DecimalToBit<'1'>]
  : Z extends [infer xc extends IntBitString, infer xm extends IntBitString]
    ? xc extends DecimalToBit<'0'>
      ? [xc, DecimalToBit<'1'>]
    : xm extends DecimalToBit<'0'>
      ? DivByZero
    : Z
  : never
