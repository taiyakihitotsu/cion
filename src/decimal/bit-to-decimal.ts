import type { D10, D100, D1000, D10000 } from './const.js'
import type { PeanoToDecimal } from './peano-to-decimal.js'
import type { BitSub, BitLTE, BitFill, BitAdd, PosMax } from '../bit/index.js'
import type { inc, T16, PeanoNumber } from '../peano.js'

type DigitTable = ['1', D10, D100, D1000, D10000]
type _DigitKeys = [4,3,2,1,0]
type DigitKidx = 0|1|2|3|4 

type _BitToDecimal<
  S extends string
, Ret extends string = '0'
, Keys = _DigitKeys
, Cul extends PeanoNumber = null> =
  Keys extends [infer K, ...infer R]
    ? BitSub<S, DigitTable[K extends DigitKidx ? K : never]> extends infer u
      ? BitLTE<u extends string ? u : never,'0'> extends true
        ? _BitToDecimal<S, `${Ret}${PeanoToDecimal<Cul>}`, R>
      : _BitToDecimal<u extends string ? u : never, Ret, Keys, inc<Cul>>
    : never
  : Ret extends string
    ? Ret
  : never

type TrimZero<
  S extends string> =
  S extends `${infer H}${infer T}`
    ? H extends '0'
      ? TrimZero<T>
    : S
  : S

export type rBitToDecimal<
  S extends string> =
  _BitToDecimal<S> extends string & infer s
    ? TrimZero<s extends string ? s : never> extends infer trimed
      ? trimed extends ''
        ? '0'
      : trimed
    : never
  : never

/**
Convert a bit-string to a decimal-string.
Supports 16-bit signed integer logic currently.

Shorter bit-strings are zero-padded to 16 bits.
See `Expected_MIN` vs `Expected_Fill`.

```typescript
type Expected_7 = BitToDecimal<'111'> // => "7"
type Expected_neg_1 = BitToDecimal<'1111111111111111'> // => "-1"
type Expected_MAX = BitToDecimal<'0111111111111111'> // => "32767"
type Expected_MIN = BitToDecimal<'1000000000000000'> // => "-32768"
type Expected_Fill = BitToDecimal<'100000000000'> // => "2048"
```
*/
export type BitToDecimal<
  S extends string> =
  BitFill<S, T16> extends `${infer H}${infer T}`
    ? H extends '1'
      ? `-${rBitToDecimal<BitAdd<'1', BitSub<PosMax, `0${T}`>>>}`
    : rBitToDecimal<S>
  : never
