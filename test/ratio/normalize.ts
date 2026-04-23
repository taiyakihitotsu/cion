import * as ratio from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

const ratio_str_test_0 : true = {} as Equal<'-5', ratio.RatioStr<ratio.Normalize<[de.DecimalToBit<'-10'>, de.DecimalToBit<'2'>]>>>
const ratio_str_test_1 : true = {} as Equal<'5',  ratio.RatioStr<ratio.Normalize<[de.DecimalToBit<'-10'>, de.DecimalToBit<'-2'>]>>>
const ratio_str_test_2 : true = {} as Equal<'-10/3', ratio.RatioStr<ratio.Normalize<[de.DecimalToBit<'-10'>, de.DecimalToBit<'3'>]>>>
const ratio_str_test_3 : true = {} as Equal<'1/5',   ratio.RatioStr<ratio.Normalize<[de.DecimalToBit<'-2'>, de.DecimalToBit<'-10'>]>>>
