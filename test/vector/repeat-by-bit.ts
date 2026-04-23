import type { RepeatByBit } from '../../src/vector/index.js'
import type { Equal } from '../../src/util.js'

const bitrepeat_test_0: true = {} as Equal<RepeatByBit<'0000000000000011', ['2']>, [['2'], ['2'], ['2']]>
const bitrepeat_test_1: true = {} as Equal<RepeatByBit<'0000000000000000', ['2']>, []>


