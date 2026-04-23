import type { T0, dec, PeanoNumber } from '../peano.js'

export type BitCut<
  B extends string
, P extends PeanoNumber = T0> =
  P extends T0
    ? B
  : B extends `${infer _H}${infer T}`
    ? BitCut<T, dec<P>>
  : never
