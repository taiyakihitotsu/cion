import type { IntBitString, Ratio } from './const.js'
import type { ForceNat } from './force-nat.js'

// [note]
// This uses `BitDiv` which truncates the decimal part, so it behaves like a step function.
// [note]
// this is trunc, not floor.
export type Trunc<Z extends IntBitString | Ratio> = ForceNat<Z>
