import * as ratio from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

const gcd_test_0 : true = {} as Equal<'21', de.BitToDecimal<ratio.GCD<de.DecimalToBit<'21'>, de.DecimalToBit<'21'>>>>
const gcd_test_1 : true = {} as Equal<'1',  de.BitToDecimal<ratio.GCD<de.DecimalToBit<'7'>, de.DecimalToBit<'5'>>>>
const gcd_test_2 : true = {} as Equal<'7',  de.BitToDecimal<ratio.GCD<de.DecimalToBit<'7'>, de.DecimalToBit<'0'>>>>
const gcd_test_3 : true = {} as Equal<'3',  de.BitToDecimal<ratio.GCD<de.DecimalToBit<'9'>, de.DecimalToBit<'30'>>>>
