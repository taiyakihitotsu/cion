import type { BitUniform } from './bit-uniform.js'
import type { BitZero } from './const.js'
import type { BitEq } from './bit-eq.js'

export type BitIsZero<
  B extends string> =
  BitUniform<BitZero, B> extends [
  infer Z,
  infer U extends string,
]
    ? BitEq<BitZero, U>
  : never
