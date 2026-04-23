import type { InterByBit } from '../../src/vector/index.js'
import type { Equal } from '../../src/util.js'

type BaseVector = [0, 1, 2]

type InterByBitFirst = InterByBit<BaseVector, '001', '010'>
const bitinter_test_0: true = {} as Equal<InterByBitFirst, [1, 2]>

type InterByBitNone = InterByBit<BaseVector, '001', '001'>
const bitinter_test_1: true = {} as Equal<InterByBitNone, [1]>

type InterByBitLessThanZero = InterByBit<BaseVector, '1000000000000001', '1111111111111111'>
const bitinter_test_2: true = {} as Equal<InterByBitLessThanZero, []>

type InterByBitLength = InterByBit<BaseVector, '000', '010'>
const bitinter_test_3: true = {} as Equal<InterByBitLength, [0, 1, 2]>

type InterByBitGreaterThanLength = InterByBit<BaseVector, '000', '11111'>
const bitinter_test_4: true = {} as Equal<InterByBitGreaterThanLength, BaseVector>
