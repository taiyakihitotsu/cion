import { RatioNot, RatioStr } from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

const not_test_0 : true = {} as Equal<'-9', de.BitToDecimal<RatioNot<de.DecimalToBit<'9'>>>>
const not_test_1 : true = {} as Equal<'1/3', RatioStr<RatioNot<[de.DecimalToBit<'-3'>, de.DecimalToBit<'9'>]>>>

const RevRatio_Expected_neg1: true = {} as Equal<["1111111111111111", "0000000000000001"], RatioNot<["0000000000000001", "0000000000000001"]>>
const RevRatio_Expected_pos1: true = {} as Equal<["0000000000000001", "0000000000000001"], RatioNot<["1111111111111111", "0000000000000001"]>>
const RevBit_Expected_neg1: true = {} as Equal<"1111111111111111", RatioNot<"0000000000000001">>
const RevBit_Expected_pos1: true = {} as Equal<"0000000000000001", RatioNot<"1111111111111111">>
const BitZero_Expected_BitZero: true = {} as Equal<"0000000000000000", RatioNot<"0000000000000000">>
const RatioZero_Expected_RatioZero: true = {} as Equal<["0000000000000000", "0000000000000001"], RatioNot<["0000000000000000", "1111111111111111"]>>
