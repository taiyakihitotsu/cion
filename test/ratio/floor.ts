import * as ratio from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

const floor_test_0 : true = {} as Equal<'5', de.BitToDecimal<ratio.Floor<[de.DecimalToBit<'11'>, de.DecimalToBit<'2'>]>>>
const floor_test_1 : true = {} as Equal<'-6', de.BitToDecimal<ratio.Floor<[de.DecimalToBit<'11'>, de.DecimalToBit<'-2'>]>>>
const floor_test_2 : true = {} as Equal<'-1', de.BitToDecimal<ratio.Floor<[de.DecimalToBit<'-3'>, de.DecimalToBit<'10'>]>>>
