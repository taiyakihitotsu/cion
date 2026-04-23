import type { Ratio, IntBitString } from './const.js'
import type { BitToDecimal } from '../decimal/index.js'

/**
Converts bit-string ratio(s) into decimal string representation.
Supports both a single ratio tuple and a pair of ratio tuples.

```typescript
DecimalRatio<["0000000000000100", "0000000000000000"]> //=> ['4', '0']
```
*/
export type DecimalRatio<
  Z extends [Ratio, Ratio] | Ratio> =
  Z extends [ [ infer xc extends IntBitString
              , infer xm extends IntBitString ]
            , [ infer yc extends IntBitString
              , infer ym extends IntBitString ] ]
    ? [ [BitToDecimal<xc>, BitToDecimal<xm>]
      , [BitToDecimal<yc>, BitToDecimal<ym>]]
  : Z extends [ infer xc extends IntBitString
              , infer xm extends IntBitString]
    ? [ BitToDecimal<xc>, BitToDecimal<xm> ]
  : never
