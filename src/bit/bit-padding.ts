import type * as Peano from "../peano.js";

export type BitPadding<
  B extends string
, P extends Peano.PeanoNumber = Peano.T0
, F extends "0" | "1" = "0"> =
  P extends Peano.T0
    ? B
  : F extends "0" | "1"
    ? BitPadding<`${F}${B}`, Peano.dec<P>, F>
  : never
