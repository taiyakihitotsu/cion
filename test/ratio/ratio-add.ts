import * as ratio from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

const arith_test_add : true = {} as Equal<['50', '6'], ratio.DecimalRatio<ratio.RatioAdd<[de.DecimalToBit<'10'>, de.DecimalToBit<'2'>], [de.DecimalToBit<'10'>, de.DecimalToBit<'3'>]>>>
