import type { TakeByBit } from '../../src/vector/index.js'
import type { Equal } from '../../src/util.js'

type BaseVector = [0, 1, 2]

type TakeByBitFirst = TakeByBit<'001', BaseVector>
const bittake_test_0: true = {} as Equal<TakeByBitFirst, [0]>

type TakeByBitNone = TakeByBit<'000', BaseVector>
const bittake_test_1: true = {} as Equal<TakeByBitNone, []>

type TakeByBitLessThanZero = TakeByBit<'1000000000000001', BaseVector>
const bittake_test_2: true = {} as Equal<TakeByBitLessThanZero, []>

type TakeByBitLength = TakeByBit<'010', BaseVector>
const bittake_test_3: true = {} as Equal<TakeByBitLength, [0, 1]>

type TakeByBitGreaterThanLength = TakeByBit<'11111', BaseVector>
const bittake_test_4: true = {} as Equal<TakeByBitGreaterThanLength, BaseVector>
