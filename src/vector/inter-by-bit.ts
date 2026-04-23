import type { BitInc } from '../bit/index.js'
import type { DropByBit } from './drop-by-bit.js'
import type { TakeByBit } from './take-by-bit.js'

/**
Returns a subset of the tuple from index N to M (inclusive of N and M).

Similar to Clojure's `subvec` which excludes M against `InterByBit`:
https://clojuredocs.org/clojure.core/subvec

It first takes elements up to M+1, then drops the first N elements.

```typescript
type Res = InterByBit<[0, 1, 2, 3, 4], "0010", "0100"> //=> [2, 3, 4]
```
*/
export type InterByBit<
  V extends unknown[]
, N extends string
, M extends string> =
DropByBit<N, TakeByBit<BitInc<M>, V>>
