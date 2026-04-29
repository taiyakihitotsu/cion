import type { BitLT } from './bit-lt.js'
import type { Neg1, BitZero } from './const.js'
import type { BitMul } from './bit-mul.js'

/**
Returns the absolute value of a bit-string.

```typescript
type Expected_1 = BitAbs<"1111111111111111"> //=> "0000000000000001"
```
*/
export type BitAbs<B extends string> = BitLT<B, BitZero> extends true ? BitMul<B, Neg1> : B
