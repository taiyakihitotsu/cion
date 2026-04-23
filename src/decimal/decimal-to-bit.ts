import type { gethan, min, dec } from '../peano.js'
import type { Equal } from '../util.js'
import type { PeanoLimited } from './const.js'
import type { StrLen } from './str-len.js'
import type { BitSub, BitAdd, NegMin } from '../bit/index.js'
import type { PeanoToDecimal } from './peano-to-decimal.js'
import type { DecimalTables } from './decimal-tables.js'

// note : case of signed 16 bit 
type DeciMaxes = ['3', '2', '7', '6', '7']
type DeciOvers =
['4' | '5' | '6' | '7' | '8' | '9',
		  '3' | '4' | '5' | '6' | '7' | '8' | '9',
		  '8' | '9',
		  '7' | '8' | '9',
		  '8' | '9']
type Digit1 = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

type DecimalToBitError0 = 'DecimalToBitError0'
type DecimalToBitError1 = 'DecimalToBitError1'
type DecimalToBitError2 = 'DecimalToBitError2'
type DecimalToBitError3 = 'DecimalToBitError3'
type DecimalToBitError4 = 'DecimalToBitError4'
type DecimalToBitError11 = 'DecimalToBitError11'

export type ExceedErrorMessage = 'exceeds the limit of a 16-bit signed integer.'

export type _DecimalToBit<
  S extends string
, IsLimited extends boolean = false
, Ret extends string = '0'> =
  true extends gethan<StrLen<S>, [PeanoLimited]>
    ? { error: DecimalToBitError0
      , message: ExceedErrorMessage }
  : true extends Equal<StrLen<S>, PeanoLimited> | IsLimited
    ? S extends `${infer F}${infer R}`
      ? F extends DeciOvers[PeanoToDecimal<min<PeanoLimited, StrLen<S>>>]
        ? { error: DecimalToBitError0
          , message: ExceedErrorMessage }
      : F extends Digit1
        ? PeanoToDecimal<dec<StrLen<S>>> extends infer D
          ? _DecimalToBit<R, F extends DeciMaxes[PeanoToDecimal<min<PeanoLimited, StrLen<S>>>] ? true : false, BitAdd<Ret, D extends keyof DecimalTables ? F extends keyof DecimalTables[D] ? DecimalTables[D][F] : '' : ''>>
        : never
      : never
    : Ret
  : S extends `${infer F}${infer R}`
    ? F extends Digit1
      ? PeanoToDecimal<dec<StrLen<S>>> extends infer D
        ? _DecimalToBit<R, false, BitAdd<Ret, D extends keyof DecimalTables ? F extends keyof DecimalTables[D] ? DecimalTables[D][F] : 'never' : 'never'>>
      : never
    : never
  : Ret

/**
Converts a decimal string to a 16-bit binary string.

- Supports signed 16-bit integer range
- Returns an error object if the input exceeds this range

```typescript
type Expected_error = DecimalToBit<'99999'> //=> { error: DecimalToBitError0, message: 'exceeds the limit of a 16-bit signed integer.' }
type Expected_20 = DecimalToBit<'20'> //=> "0000000000010100"
type Expected_Neg1 = DecimalToBit<'-1'> //=> "1111111111111111"
```
*/
export type DecimalToBit<
  S extends string> =
  S extends `${infer H}${infer T}`
    ? H extends '-'
      ? _DecimalToBit<T> extends infer retT
        ? retT extends string
          ? BitSub<NegMin, retT> extends `${infer _}${infer rT}`
            ? `1${rT}`
          : never
        : retT
      : never
    : _DecimalToBit<S>
  : DecimalToBitError11
