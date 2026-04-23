import type { min } from '../peano.js'
import type { BitLenGthan } from './bit-len-gt.js'
import type { BitPadding } from './bit-padding.js'
import type { BitLen } from './bit-len.js'

export type BitUniform<
  B extends string
, C extends string> =
  BitLenGthan<
  B,
  C
> extends true
    ? [B, BitPadding<C, min<BitLen<B>, BitLen<C>>>]
  : [BitPadding<B, min<BitLen<C>, BitLen<B>>>, C]
