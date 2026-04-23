import type { IntBitString, Ratio } from './const.js'
import type { Scaling } from './scaling.js'
import type { BitSub } from '../bit/index.js'

/**
Subtracts the second ratio tuple from the first.

NOTE: The resulting ratio is NOT reduced (simplified) by `Scaling` automatically.
```typescript
type Expected_2 = RatioSub<['101', '10'], ['11', '10']> //=> ["0000000000000010", "10"]
```
*/
export type RatioSub<
  X extends Ratio
, Y extends Ratio> =
  Scaling<X, Y> extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                        , [ infer yc extends IntBitString
                            , infer _ extends IntBitString ] ]
    ? [BitSub<xc, yc>, xm]
  : never
