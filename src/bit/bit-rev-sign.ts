import type { Neg1 } from './const.js'

import type { BitMul } from './bit-mul.js'

export type BitRevSign<
  S extends string> =
BitMul<S, Neg1>
