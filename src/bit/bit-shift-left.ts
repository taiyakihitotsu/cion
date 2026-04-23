import type * as Peano from "../peano.js"
import type { BitShiftLeftOne } from "./bit-shift-left-one.js"

export type BitShiftLeft<
  B extends string
, N extends Peano.PeanoNumber> =
  N extends Peano.T0
    ? B
  : BitShiftLeft<BitShiftLeftOne<B>, Peano.dec<N>>
