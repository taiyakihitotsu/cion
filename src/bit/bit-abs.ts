import type { BitLT } from './bit-lt.js'
import type { Neg1, BitZero } from './const.js'
import type { BitMul } from './bit-mul.js'

export type BitAbs<B extends string> = BitLT<B, BitZero> extends true ? BitMul<B, Neg1> : B
