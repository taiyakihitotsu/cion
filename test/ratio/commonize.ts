import type * as de from "../../src/decimal/index.js"
import type * as ratio from "../../src/ratio/index.js"
import type { Equal } from "../../src/util.ts"

const commonize_test_0 : true = {} as Equal<['3', '1'], ratio.DecimalRatio<ratio.Commonize<[de.DecimalToBit<'9'>, de.DecimalToBit<'3'>]>>>
const commonize_test_1 : true = {} as Equal<['1', '2'], ratio.DecimalRatio<ratio.Commonize<[de.DecimalToBit<'5'>, de.DecimalToBit<'10'>]>>>
const commonize_test_2 : true = {} as Equal<['-1', '3'], ratio.DecimalRatio<ratio.Commonize<[de.DecimalToBit<'10'>, de.DecimalToBit<'-30'>]>>>
const commonize_test_3 : true = {} as Equal<['1', '1'], ratio.DecimalRatio<ratio.Commonize<[de.DecimalToBit<'-7'>, de.DecimalToBit<'-7'>]>>>
const commonize_test_4 : true = {} as Equal<['0', '1'], ratio.DecimalRatio<ratio.Commonize<[de.DecimalToBit<'0'>, de.DecimalToBit<'5'>]>>>
