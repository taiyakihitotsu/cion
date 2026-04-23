import type { MAX } from './const.js'
import type { dec, PeanoNumber } from '../peano.js'

import type { BitFill } from './bit-fill.js'
import type { BitShiftLeft } from './bit-shift-left.js'
import type { BitAdd } from './bit-add.js'

export type BitMul<
  B extends string
, C extends string
, M extends PeanoNumber = MAX
, R extends string = BitFill<"0", M>
, tB extends string = BitFill<B, M>
, tC extends string = BitFill<C, M>
, N extends PeanoNumber = Extract<dec<M>, PeanoNumber>> =
  tB extends `${infer H}${infer T}`
    ? H extends "/"
      ? R
    : H extends "0"
      ? BitMul<B, C, M, R, `${T}/`, tC, dec<N>>
    : H extends "1"
      ? BitMul<
            B,
            C,
            M,
            BitAdd<R, BitShiftLeft<tC, N>>,
            `${T}/`,
            tC,
            dec<N>
          >
    : never
  : never
