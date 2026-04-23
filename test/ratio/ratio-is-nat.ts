import type { Equal } from '../../src/util.js'
import type { IsNat } from '../../src/ratio/index.js'

const One_Expected_true: true = {} as Equal<true, IsNat<"0000000000000001">>
const Zero_Expected_true: true = {} as Equal<true, IsNat<"0000000000000000">>
const NegOne_Expected_true: true = {} as Equal<false, IsNat<"1111111111111111">>
