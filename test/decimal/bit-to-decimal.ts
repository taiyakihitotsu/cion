import type {Equal} from '../../src/util.js'
import type { BitToDecimal, DecimalToBit } from '../../src/decimal/index.js'
import type { BitDiv } from '../../src/bit/index.js'

const bittodecimal_test_0: true = {} as Equal<BitToDecimal<'1010'>, '10'>
const bittodecimal_test_1: true = {} as Equal<BitToDecimal<'0001'>, '1'>
const bittodecimal_test_2: true = {} as Equal<BitToDecimal<'0000'>, '0'>
const bittodecimal_test_3: true = {} as Equal<BitToDecimal<`0111110101101111`>, '32111'>
const bittodecimal_test_4: true = {} as Equal<BitToDecimal<'0111111111111111'>, '32767'>
const bittodecimal_test_5: true = {} as Equal<BitToDecimal<`1111110101101111`>, '-657'>
const bittodecimal_test_6: true = {} as Equal<BitToDecimal<'1111111111111111'>, '-1'>

const test0: BitToDecimal<BitDiv<DecimalToBit<'11'>, DecimalToBit<'2'>>> = '5'
const test1: BitToDecimal<BitDiv<DecimalToBit<'11'>, DecimalToBit<'20'>>> = '0'
const test2: BitToDecimal<BitDiv<DecimalToBit<'10'>, DecimalToBit<'3'>>> = '3'
const test3: BitToDecimal<BitDiv<DecimalToBit<'10'>, DecimalToBit<'-3'>>> = '-3'
