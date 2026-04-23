import * as ratio from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

const trunc_test_0 : true = {} as Equal<'5', de.BitToDecimal<ratio.Trunc<[de.DecimalToBit<'11'>, de.DecimalToBit<'2'>]>>>
const trunc_test_1 : true = {} as Equal<'-5', de.BitToDecimal<ratio.Trunc<[de.DecimalToBit<'11'>, de.DecimalToBit<'-2'>]>>>
const trunc_test_2 : true = {} as Equal<'0', de.BitToDecimal<ratio.Trunc<[de.DecimalToBit<'-3'>, de.DecimalToBit<'10'>]>>>
