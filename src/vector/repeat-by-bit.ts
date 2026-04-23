import type { BitDec, BitGTE } from '../bit/index.js'
import type { Rec, _rec } from '../util.js'

/**
Creates a tuple containing N copies of value V.
Uses a recursive process (via `Rec`) to handle large bit-string counts.
Equivalent to Clojure's `repeat`.

```typescript
type Res = RepeatByBit<"0011", "a"> //=> ["a", "a", "a"] (if "0011" is 3)
```
*/
export type RepeatByBit<
  N extends string
, V> =
Extract<Rec<_rec<_RepeatByBit<N, V>>>, unknown[]> // [todo] check

/**
Internal recursive helper for RepeatByBit.
Decrements the bit-string count N until it reaches zero.
*/
type _RepeatByBit<
  N extends string
, V
, R extends V[] = []> =
  BitGTE<'0000000000000000',N> extends true
    ? { r: R }
  : { r: _RepeatByBit<BitDec<N>, V, [...R, V]> }
