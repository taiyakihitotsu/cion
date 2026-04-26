import type { DropByBit } from '../../src/vector/index.js'
import type { Equal } from '../../src/util.js'

type BaseVector = [0, 1, 2]

type DropByBitFirst = DropByBit<'001', BaseVector>
const bitdrop_test_0: true = {} as Equal<DropByBitFirst, [1, 2]>

type DropByBitNone = DropByBit<'000', BaseVector>
const bitdrop_test_1: true = {} as Equal<DropByBitNone, BaseVector>

type DropByBitLessThanZero = DropByBit<'1000000000000001', BaseVector>
const bitdrop_test_2: true = {} as Equal<DropByBitLessThanZero, BaseVector>

type DropByBitLength = DropByBit<'010', BaseVector>
const bitdrop_test_3: true = {} as Equal<DropByBitLength, [2]>

type DropByBitGreaterThanLength = DropByBit<'1111111111111111', BaseVector>
const bitdrop_test_4: true = {} as Equal<DropByBitGreaterThanLength, BaseVector>
