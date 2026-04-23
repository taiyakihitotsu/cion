import type { BitSub } from './bit-sub.js'

export type BitDec<B extends string> = BitSub<B, "0000000000000001">
