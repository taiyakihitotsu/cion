import * as ratio from '../../src/ratio/index.js'
import * as de from '../../src/decimal/index.js'
import type { Equal } from '../../src/util.js'

const simplify_test_0 : true = {} as Equal<'4',    ratio.SimplifyStr<[de.DecimalToBit<'8'>, de.DecimalToBit<'2'>]>>
const simplify_test_1 : true = {} as Equal<'8/3',  ratio.SimplifyStr<[de.DecimalToBit<'8'>, de.DecimalToBit<'3'>]>>
const simplify_test_2 : true = {} as Equal<'-8/3', ratio.SimplifyStr<[de.DecimalToBit<'-8'>, de.DecimalToBit<'3'>]>>
