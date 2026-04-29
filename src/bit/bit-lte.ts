import type { BitGT } from './bit-gt.ts'

/**
NOTE: `BitGT` calls `BitGTE` internally, handling bit-length normalization.
Therefore, `BitLTE` does not need to manage length differences between `B` and `C`.
*/
export type BitLTE<
  B extends string
, C extends string> =
  BitGT<B,C> extends true
    ? false
  : true
