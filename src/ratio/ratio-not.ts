import type { IntBitString, Ratio, RatioNumber } from './const.js' 
import type { BitRevSign } from '../bit/index.js'
import type { Commonize } from './commonize.js'

/**
- Return a sign-reversed bit if a bit string is passed.
- Return a sign-reversed ratio tuple if a ratio tuple is passed.

```typescript
type RevRatio_Expected_neg1 = RatioNot<["0000000000000001", "0000000000000001"]> //=> ["1111111111111111", "0000000000000001"]
type RevRatio_Expected_pos1 = RatioNot<["1111111111111111", "0000000000000001"]> //=> ["0000000000000001", "0000000000000001"]
type RevBit_Expected_neg1 = RatioNot<"0000000000000001"> //=> "1111111111111111"
type RevBit_Expected_pos1 = RatioNot<"1111111111111111"> //=> "0000000000000001"
type BitZero_Expected_BitZero = RatioNot<"0000000000000000"> //=> "0000000000000000"
type RatioZero_Expected_RatioZero = RatioNot<["0000000000000000", "1111111111111111"]> //=> ["0000000000000000", "0000000000000001"]
```
*/
export type RatioNot<
  X extends RatioNumber> =
  X extends IntBitString
    ? BitRevSign<X>
  : X extends Ratio & [infer t  extends IntBitString, infer d extends IntBitString]
    ? Commonize<[BitRevSign<t>, d]>
  : never
