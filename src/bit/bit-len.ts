import type * as Peano from "../peano.js"

export type BitLen<
  B extends string
, count extends Peano.PeanoNumber = Peano.T0> =
  B extends `${infer HB}${infer TB}`
    ? HB extends "0" | "1"
      ? BitLen<TB, Peano.inc<count>>
    : never
  : count
