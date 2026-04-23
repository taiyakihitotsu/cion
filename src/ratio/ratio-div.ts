import type { IntBitString, Ratio, DivByZero } from './const.js'
import type { BitMul, BitZero } from '../bit/index.js'

/**
Devides the first ratio tuple by the second.

NOTE: The resulting ratio is NOT reduced (simplified) by `Scaling` automatically.
```typescript
type Expected_24_12 = RatioDiv<['1000', '11'], ['100', '11']> //=> ["0000000000011000", "0000000000001100"]
```
*/
export type RatioDiv<
  X extends Ratio
, Y extends Ratio> =
  [X, Y] extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                        , [ infer yc extends IntBitString
                            , infer ym extends IntBitString ] ]
    ? BitZero extends yc | xm
      ? DivByZero
    : [BitMul<xc, ym>, BitMul<xm, yc>]
  : never
