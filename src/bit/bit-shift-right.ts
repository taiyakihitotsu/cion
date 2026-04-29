import type { MAX } from './const.js'
import type { T0, T1, min, dec, PeanoNumber  } from '../peano.js'
import type { BitPadding } from "./bit-padding.js"

export type BitShiftRight<
  B extends string
, M extends PeanoNumber = MAX
, N = T1
> =
  min<M, N> extends infer Comp
    ? [Comp] extends [never]
      ? BitPadding<"0", dec<M>>
    : Comp extends T0
      ? BitPadding<"0", dec<M>>
    : _BitShiftRight<B, M, N>
  : never

type _BitShiftRight<
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

