import type { IntBitString, Ratio, RatioNumber } from './const.js'
import type { BitIsZero } from '../bit/index.js'

/**
Returns `true` if the argument is a bit or ratio-tuple expressing `0`.

```typescript
type Bit_Expected_true = RatioIsZero<"0000000000000000"> //=> true
type Ratio_Expected_true = RatioIsZero<["0000000000000000", "0000000000000001"]> //=> true
type Ratio_Commonized_Expected_true = RatioIsZero<["0000000000000000", "1111111111111111"]> //=> true
type BitOne_Expected_true = RatioIsZero<"0000000000000001"> //=> false
```
*/
export type RatioIsZero<
  X extends RatioNumber> =
  X extends IntBitString
    ? BitIsZero<X>
  : X extends Ratio & [infer t extends IntBitString, infer _]
    ? BitIsZero<t>
  : never
