import type { BitGTE } from './bit-gte.js'

/**
NOTE: `BitGTE` internally handles bit-length normalization.
Therefore, `BitLT` does not need to manage length differences between `B` and `C`.
*/
export type BitLT<
  B extends string
, C extends string> =
  BitGTE<B,C> extends true
    ? false
  : true
