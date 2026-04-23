import * as ratio from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

const force_test_0 : true = {} as Equal<['10', '2'], ratio.DecimalRatio<ratio.ForceRatio<[de.DecimalToBit<'10'>, de.DecimalToBit<'2'>]>>>
const force_test_1 : true = {} as Equal<['3', '1'],  ratio.DecimalRatio<ratio.ForceRatio<de.DecimalToBit<'3'>>>>
