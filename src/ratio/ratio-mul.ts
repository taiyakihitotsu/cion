import type { IntBitString, Ratio, RatioZero } from './const.js'
import type { BitIsZero, BitMul } from '../bit/index.js'

/**
Multiply the second ratio tuple from the first.

NOTE: The resulting ratio is NOT reduced (simplified) by `Scaling` automatically.
```typescript
 type Expected_2 = RatioMul<['101', '10'], ['10', '10']> //=> ["0000000000001010", "0000000000000100"]
```
*/
export type RatioMul<
  X extends Ratio
, Y extends Ratio> =
  [X, Y] extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                        , [ infer yc extends IntBitString
                            , infer ym extends IntBitString ] ]
    ? BitIsZero<xc> | BitIsZero<yc> extends false
      ? [ BitMul<xc, yc>
        , BitMul<xm, ym>]
    : RatioZero
  : never
