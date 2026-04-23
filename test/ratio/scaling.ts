import * as ratio from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

type Expected_15_dominators = ratio.Scaling<["1000", "0011"], ["0100", "0101"]> //=> [["0000000000101000", "00000000000000001111"], ["0000000000001100", "00000000000000001111"]]
const expected_15_dominators: true = {} as Equal<[["0000000000101000", "0000000000001111"], ["0000000000001100", "0000000000001111"]], Expected_15_dominators>

const ratio_scale_test_0 : true = {} as Equal<[['10', '2'], ['8', '2']], ratio.DecimalRatio<ratio.Scaling<[de.DecimalToBit<'10'>, de.DecimalToBit<'2'>], [de.DecimalToBit<'8'>, de.DecimalToBit<'2'>]>>>
const ratio_scale_test_1 : true = {} as Equal<[['30', '6'], ['16', '6']], ratio.DecimalRatio<ratio.Scaling<[de.DecimalToBit<'10'>, de.DecimalToBit<'2'>], [de.DecimalToBit<'8'>, de.DecimalToBit<'3'>]>>>
const ratio_scale_test_2 : true = {} as Equal<[['-10', '2'], ['8', '2']], ratio.DecimalRatio<ratio.Scaling<[de.DecimalToBit<'-10'>, de.DecimalToBit<'2'>], [de.DecimalToBit<'8'>, de.DecimalToBit<'2'>]>>>
