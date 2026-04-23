import type { BitGT } from './bit-gt.ts'

export type BitLTE<
  B extends string
, C extends string> =
  BitGT<B,C> extends true
    ? false
  : true
