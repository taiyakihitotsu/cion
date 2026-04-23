import type { BitAdd } from './bit-add.js'

export type BitInc<B extends string> = BitAdd<B, "0000000000000001">
