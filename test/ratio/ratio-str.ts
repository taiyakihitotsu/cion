import * as ratio from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

type Expected_0 = ratio.RatioStr<['0000000000000000', '0000000000000001']>
const expected_0: true = {} as Equal<'0', Expected_0>

type Expected_neg1 = ratio.RatioStr<['1111111111111111', '0000000000000001']>
const expected_neg1: true = {} as Equal<'-1', Expected_neg1>

type Expected_not_scale = ratio.RatioStr<['0000000000000100', '0000000000000010']>
const expected_not_scale: true = {} as Equal<'4/2', Expected_not_scale>

// 0 denominator isn't rejected by RatioStr.
type Expected_0_denominator = ratio.RatioStr<['0000000000000100', '0000000000000000']>
const expected_0_denominator: true = {} as Equal<'4/0', Expected_0_denominator>
