import * as ratio from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

const force_nat_test_0 : true = {} as Equal<'5', de.BitToDecimal<ratio.ForceNat<[de.DecimalToBit<'10'>, de.DecimalToBit<'2'>]>>>
const force_nat_test_1 : true = {} as Equal<'3', de.BitToDecimal<ratio.ForceNat<[de.DecimalToBit<'10'>, de.DecimalToBit<'3'>]>>>

