import type { Ratio, IntBitString } from './const.js'
import type { BitMul } from '../bit/index.js'

/**
Normalizes two ratios to a common denominator.

```typescript
type Expected_15_dominators = Scaling<["1000", "0011"], ["0100", "0101"]> //=> [["0000000000101000", "00000000000000001111"], ["0000000000001100", "00000000000000001111"]]
```
*/
export type Scaling<
  X extends Ratio
, Y extends Ratio> =
  [X, Y] extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                 , [ infer yc extends IntBitString
                            , infer ym extends IntBitString ] ]
    ? xm extends ym
      ? [X, Y]
    : BitMul<xm, ym> extends infer SM
      ? [ [BitMul<xc, ym>, SM]
        , [BitMul<yc, xm>, SM]]
    : never
  : never
