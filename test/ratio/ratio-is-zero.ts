import type { RatioIsZero } from '../../src/ratio/index.js'
import type { Equal } from '../../src/util.js'

const Bit_Expected_true: true = {} as Equal<true, RatioIsZero<"0000000000000000">>
const Ratio_Expected_true: true = {} as Equal<true, RatioIsZero<["0000000000000000", "0000000000000001"]>>
const Ratio_Commonized_Expected_true: true = {} as Equal<true, RatioIsZero<["0000000000000000", "1111111111111111"]>>
const BitOne_Expected_true: true = {} as Equal<false, RatioIsZero<"0000000000000001">>
