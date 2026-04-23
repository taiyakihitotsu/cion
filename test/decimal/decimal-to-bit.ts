import type {Equal} from '../../src/util.js'
import type { DecimalToBit, ExceedErrorMessage } from '../../src/decimal/index.js'

const decimaltobit_test_0: true = {} as Equal<DecimalToBit<'32111'>, `${0}111110101101111`>
const decimaltobit_test_1: true = {} as Equal<DecimalToBit<'39000'>, {error: 'DecimalToBitError0', message: ExceedErrorMessage}>
const decimaltobit_test_2: true = {} as Equal<DecimalToBit<'666666'>, {error: 'DecimalToBitError0', message: ExceedErrorMessage}>
const decimaltobit_test_3: true = {} as Equal<DecimalToBit<'8'>, `0000000000001000`>
const decimaltobit_test_4: true = {} as Equal<DecimalToBit<'9'>, `0000000000001001`>
const decimaltobit_test_5: true = {} as Equal<DecimalToBit<'-9'>, '1111111111110111'>
const decimaltobit_test_6: true = {} as Equal<DecimalToBit<'0'>, '0000000000000000'>
const decimaltobit_test_7: true = {} as Equal<DecimalToBit<'-1'>, '1111111111111111'>
const decimaltobit_test_8: true = {} as Equal<DecimalToBit<'-111111'>, {error: 'DecimalToBitError0', message: ExceedErrorMessage}>
const decimaltobit_test_9: true = {} as Equal<DecimalToBit<'32767'>, "0111111111111111">
const decimaltobit_test_10: true = {} as Equal<DecimalToBit<'32768'>, {error: 'DecimalToBitError0', message: ExceedErrorMessage}>
const decimaltobit_test_11: true = {} as Equal<DecimalToBit<'-32768'>, {error: 'DecimalToBitError0', message: ExceedErrorMessage}>
const decimaltobit_test_12: true = {} as Equal<DecimalToBit<'-32767'>, "1000000000000001">
