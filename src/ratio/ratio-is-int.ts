import type { IntBitString, Ratio, IntOne, RatioNumber } from './const.js'
import type { Commonize } from './commonize.js'
import type { IsBitExpr } from "../decimal/index.js"

/**
Return `true` if argument is:

- a string literal
- a ratio tuple which can be converted to integer string.

```typescript
type Bit_Expected_true = IsInt<"00010"> //=> true
type Empty_Expected_false = IsInt<""> //=> false
type NotBit_Expected_false = IsInt<"-1"> //=> false
type Ratio_Expected_true = IsInt<["00010", "00010"]> //=> true
type Ratio_Both_Empty_Expected_false = IsInt<["", ""]> //=> false
type Ratio_Left_Empty_Expected_false = IsInt<["", "00010"]> //=> false
type Ratio_Right_Empty_Expected_false = IsInt<["00010", ""]> //=> false
type NotRatio_Both_Expected_false = IsInt<["-1", "-1"]> //=> false
type NotRatio_Left_Expected_false = IsInt<["-1", "00010"]> //=> false
type NotRatio_Right_Expected_false = IsInt<["00010", "-1"]> //=> false
```
*/
export type IsInt<
  X extends RatioNumber> =
  X extends IntBitString
    ? IsBitExpr<X>
  : X extends Ratio
    ? [IsBitExpr<X[0]>, IsBitExpr<X[1]>] extends [true, true]
      ? Commonize<X> extends [infer _t, infer u extends IntBitString]
	? u extends IntOne
	  ? true
	: false
      : false
    : false
  : false
