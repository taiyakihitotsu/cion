import type { Nat, Ratio, RatioNumber } from './const.js'
import type { BitToDecimal } from '../decimal/index.js'
import type { RatioStr } from './ratio-str.js'
import type { Commonize } from './commonize.js'

/**
Serializes a RatioNumber or BitNumber into a decimal string.
Automatically normalizes Ratio types to their simplest fractional string form (by `Commonize`).

```typescript
type Simplify = SimplifyStr<["0000000000001000", "0000000000000011"]> //=> "8/3"
```
*/
export type SimplifyStr<
  X extends RatioNumber> =
  X extends Nat
    ? `${BitToDecimal<X>}`
  : X extends Ratio
    ? Commonize<X> extends infer Commonized extends Ratio
      ? RatioStr<Commonized>
    : never
  : never
