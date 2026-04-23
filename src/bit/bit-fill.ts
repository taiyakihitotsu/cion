import type { min, PeanoNumber } from '../peano.js'
import type { MAX } from './const.js'
import type { BitCut } from './bit-cut.js'
import type { BitLen } from './bit-len.js'
import type { BitPadding } from './bit-padding.js'

export type BitFill<
  B extends string
, M extends PeanoNumber = MAX
, tB extends string = BitPadding<B, M>> =
BitCut<tB, min<BitLen<tB>, M>>
