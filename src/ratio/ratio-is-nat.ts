import type { Equal } from '../util.js'
import type { IsInt } from './ratio-is-int.js'
import type { Relation } from './relation.js'
import type { RatioZero, RatioNumber } from './const.js'
import type { BitOne } from '../bit/index.js'

/**
Returns `true` if the argument is a string expressing natural number.

```typescript
type One_Expected_true = IsNat<"0000000000000001"> //=> true
type Zero_Expected_true = IsNat<"0000000000000000"> //=> true
type NegOne_Expected_true = IsNat<"1111111111111111"> //=> false
```
*/
export type IsNat<
  X extends RatioNumber> =
  [Equal<true, IsInt<X>>, Equal<true, Relation<X extends string ? [X, BitOne] : X, RatioZero, '>='>>] extends [true, true]
    ? true
  : false
