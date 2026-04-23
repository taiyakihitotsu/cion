import * as ratio from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

const abs_test_0 : true = {} as Equal<'9', de.BitToDecimal<ratio.RatioAbs<de.DecimalToBit<'-9'>>>>
const abs_test_1 : true = {} as Equal<'3', ratio.RatioStr<ratio.RatioAbs<[de.DecimalToBit<'-9'>, de.DecimalToBit<'3'>]>>>
