import type { BitGTE, BitSub } from '../bit/index.js'

/**
This defines utility for tuples.
Usable for `regex-compiler.ts`.

Every Numbers are implicitly regarded as `BitString`.
So, this is not `number` but `string`.

Some types are redundant but I've leaved them just in cases.
*/

/**
`BitDrop<BitString, Tuple>`

https://clojuredocs.org/clojure.core/drop
*/
export type DropByBit<N extends string, V extends unknown[]> = _DropByBit<N, V>

type _DropByBit<
  N extends string
, V extends unknown[]> =
  V extends []
    ? V
  : true extends BitGTE<"0", N>
    ? V
  : V extends [infer _, ...infer T extends unknown[]]
    ? _DropByBit<BitSub<N, "1">, T>
  : 0
