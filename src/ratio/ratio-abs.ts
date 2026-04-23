import type { IntBitString, RatioNumber } from './const.js'
import type { Commonize } from './commonize.js'
import type { BitAbs } from '../bit/index.js'

/**
Returns an absolute value for a ratio tuple.

```typescript
type Expected_1_2 = RatioAbs<['1111111111111110', '1111111111111100']> //=> ['0000000000000001', '0000000000000010']
```
*/
export type RatioAbs<
  X extends RatioNumber> =
  X extends IntBitString
    ? BitAbs<X>
  : X extends [infer t  extends IntBitString, infer d extends IntBitString]
    ? Commonize<[BitAbs<t>, BitAbs<d>]>
  : never
