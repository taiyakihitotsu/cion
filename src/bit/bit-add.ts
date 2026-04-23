import type { BitXor } from './bit-xor.js'
import type { BitAnd } from './bit-and.js'
import type { BitShiftLeftOne } from './bit-shift-left-one.js'
import type { BitIsZero } from './bit-is-zero.js'
import type { BitUniform } from './bit-uniform.js'
import type { BitFill } from './bit-fill.js'
import type { MAX } from './const.js'
import type { PeanoNumber } from '../peano.js'

// note : unsinged
export type _BitAdd<
  B extends string
, C extends string> =
  BitXor<B, C> extends infer _Xor extends string
    ? BitAnd<B, C> extends infer _And extends string
      ? BitShiftLeftOne<_And> extends string & infer _Carry extends string
        ? BitIsZero<
          BitUniform<_Carry, "0"> extends [infer _C extends string, infer _R]
            ? _C
            : never
        > extends true
          ? _Xor
        : _BitAdd<_Xor, _Carry>
      : never
    : never
  : never

export type BitAdd<
  B extends string
, C extends string
, M extends PeanoNumber = MAX> =
  BitFill<
  B,
  M
> extends infer _tB extends string
    ? BitFill<C, M> extends infer _tC extends string
      ? _BitAdd<_tB, _tC>
    : never
  : never
