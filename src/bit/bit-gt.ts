import type { BitGTE } from './bit-gte.js'

export type BitGT<
  B extends string
, C extends string> =
  BitGTE<B,C> extends true
    ? B extends C
      ? C extends B
        ? false
      : true
    : true
  : false
