import type { BitGTE, BitSub } from '../bit/index.js'

/**
Internal recursive helper for TakeByBit.
Iteratively collects elements while decrementing the bit-string count N.
*/
type _TakeByBit<
  N extends string
, V extends unknown[]
, R extends unknown[] = []> =
  V extends []
    ? R
  : BitGTE<"0", N> extends true
    ? R
  : V extends [infer F, ...infer T]
    ? _TakeByBit<BitSub<N, "1">, T, [...R, F]>
  : []

/**
Returns a new tuple containing the first N elements of the given tuple V.
If N is "0" or the tuple is empty, it returns an empty tuple [].

Equivalent to Clojure's `take`.

https://clojuredocs.org/clojure.core/take

```typescript
type Res = TakeByBit<"0010", [1, 2, 3, 4]> //=> [1, 2] (if "0010" is 2)
```
*/
export type TakeByBit<N extends string, V extends unknown[]> = _TakeByBit<N, V>
