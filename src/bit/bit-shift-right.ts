import type { MAX } from './const.js'
import type { T0, T1, min, dec, PeanoNumber  } from '../peano.js'

export type _BitShiftRight<
  B extends string
, M extends PeanoNumber = MAX
, N = T1
, D = min<M, N>
, R extends string = ""> =
  D extends T0
    ? R extends ""
      ? B
    : R
  : B extends `${infer H}${infer T}`
    ? _BitShiftRight<`${T}`, M, N, dec<D>, `${R}${H}`>
  : R
