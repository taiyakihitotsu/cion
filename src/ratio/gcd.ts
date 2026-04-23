import type { IntBitString } from './const.js'
import type { BitMod, BitAbs, BitLT, BitDec, BitOne, BitZero } from '../bit/index.js'

/**
Internal recursive helper for GCD.
It decrements from the smaller value until a common divisor is found.
*/
export type _GCD<
  x extends IntBitString
, y extends IntBitString
, i extends IntBitString = y> =
  i extends BitOne
    ? i
  : [ BitMod<y, i>
    , BitMod<x, i>] extends [BitZero, BitZero]
    ? i
  : _GCD<x, y, BitDec<i>>

/**
Calculates the Greatest Common Divisor (GCD) of two bit-strings.
It uses the absolute values of the inputs to ensure a positive result.
```typescript
type Expected_3 = GCD<"1001", "11110"> //=> "0000000000000011"
```
*/
export type GCD<
  X extends IntBitString
, Y extends IntBitString> =
  BitLT<X, Y> extends true
    ? _GCD<BitAbs<X>, BitAbs<Y>>
  : _GCD<BitAbs<Y>, BitAbs<X>>
