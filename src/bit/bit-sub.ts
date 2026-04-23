import type { MAX } from './const.js'
import type { BitAdd } from './bit-add.js'
import type { BitFill } from './bit-fill.js'
import type { BitNot } from './bit-not.js'
import type { PeanoNumber } from '../peano.js'

export type BitSub<
  B extends string
, C extends string
, M extends PeanoNumber = MAX> =
BitAdd<
  BitFill<B, M>,
  BitAdd<BitNot<BitFill<C, M>>, BitFill<"1", M>>
>
