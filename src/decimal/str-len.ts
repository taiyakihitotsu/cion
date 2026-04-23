import type { inc, PeanoNumber } from '../peano.js'

/**
Calculates the length of a string and returns it as a Peano number.
*/
export type StrLen<
  S extends string
, I extends PeanoNumber = null> =
  S extends `${infer _}${infer T}`
    ? StrLen<T, inc<I>>
  : I
