import type { BitGTE } from './bit-gte.js'

/**
NOTE: `BitGTE` internally handles bit-length normalization.
Therefore, `BitGT` does not need to manage length differences between `B` and `C`.
*/
export type BitGT<
  B extends string
, C extends string> =
  BitGTE<B, C> extends true
    ? B extends C
      ? C extends B
        ? false
      : true
    : true
  : false
