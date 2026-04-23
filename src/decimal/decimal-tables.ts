import type { BitMul } from '../bit/index.js'
import type { D10, D100, D1000, D10000 } from './const.js'

export type MapMul<
  V extends string[]
, B extends string
, R extends string[] = []> =
  V extends [infer H, ...infer T]
    ? BitMul<H extends string ? H : never, B> extends infer M
      ? T['length'] extends 0
        ? [...R, M]
      : MapMul<T extends string[] ? T : never, B, [...R, M extends string ? M : never]>
    : never
  : never

export type DecimalTable1 = ['0','1','10','11','100','101','110','111', '1000', '1001']
export type DecimalTable2 = MapMul<DecimalTable1,  D10>
export type DecimalTable3 = MapMul<DecimalTable1, D100>
export type DecimalTable4 = MapMul<DecimalTable1, D1000>
export type DecimalTable5 = MapMul<['00', '01', '10', '11', '00','00','00','00','00','00'], D10000>

/**
Pre-computed lookup tables for decimal-to-bit conversion.

Each entry provides the bit-string with `DecimalTables[position][digit]` for: `digit * 10 ^ position`.

To represent `20` by `(2 * 10 ^ 1)`:
- `position` is `1`
- `digit` is `2`

So return `"0000000000010100"`.

```typescript
type Expected_20 = DecimalTables[1][2] //=> "0000000000010100"
```
*/
export type DecimalTables = {0: DecimalTable1, 1:DecimalTable2, 2:DecimalTable3, 3:DecimalTable4, 4:DecimalTable5}
