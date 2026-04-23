import type { BitSub } from './bit-sub.js'

export type BitGTE<
  B extends string
, C extends string> =
  BitSub<B,C> extends `${infer H}${infer _}`
    ? H extends '1'
      ? false
    : true
  : never
