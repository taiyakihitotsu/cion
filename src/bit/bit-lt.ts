import type { BitGTE } from './bit-gte.js'

export type BitLT<
  B extends string
, C extends string> =
  BitGTE<B,C> extends true
    ? false
  : true
