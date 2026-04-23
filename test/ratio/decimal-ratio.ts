import * as ratio from '../../src/ratio/index.js'
import type { Equal } from '../../src/util.js'

type Expected_1_2 = ratio.DecimalRatio<["0000000000000001", "0000000000000010"]>
const expected_1_2: true = {} as Equal<['1', '2'], Expected_1_2>

type Expected_4_4 = ratio.DecimalRatio<["0000000000000100", "0000000000000100"]>
const expected_4_4: true = {} as Equal<['4', '4'], Expected_4_4>

type Expected_4_neg4 = ratio.DecimalRatio<["0000000000000100", "1111111111111100"]>
const expected_4_neg4: true = {} as Equal<['4', '-4'], Expected_4_neg4>

// 0 denominator is not rejected by DecimalRatio.
type Expected_by_0 = ratio.DecimalRatio<["0000000000000100", "0000000000000000"]>
const Expected_by_0: true = {} as Equal<['4', '0'], Expected_by_0>
