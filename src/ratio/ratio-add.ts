import type { IntBitString, Ratio } from './const.js'
import type { Scaling } from './scaling.js'
import type { BitAdd } from '../bit/index.js'

/**
Add the first and the second ratio tuples.

NOTE: The resulting ratio is NOT reduced (simplified) by `Scaling` automatically.
```typescript
type Expected_12_3 = RatioAdd<['1000', '11'], ['100', '11']> //=> ["0000000000001100", "11"]
```
*/
export type RatioAdd<
  X extends Ratio
, Y extends Ratio> =
  Scaling<X, Y> extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                        , [ infer yc extends IntBitString
                            , infer _ extends IntBitString ] ]
    ? [BitAdd<xc, yc>, xm]
  : never
