import type { BitRepeat, BitDrop, BitTake, BitInter, Last } from '../src/vecutil.js'
import type { Equal } from '../src/util.js'

// BitDrop
type BaseVector = [0, 1, 2]

type BitDropFirst = BitDrop<'001', BaseVector>
const bitdrop_test_0: true = {} as Equal<BitDropFirst, [1, 2]>

type BitDropNone = BitDrop<'000', BaseVector>
const bitdrop_test_1: true = {} as Equal<BitDropNone, BaseVector>

type BitDropLessThanZero = BitDrop<'1000000000000001', BaseVector>
const bitdrop_test_2: true = {} as Equal<BitDropLessThanZero, BaseVector>

type BitDropLength = BitDrop<'010', BaseVector>
const bitdrop_test_3: true = {} as Equal<BitDropLength, [2]>

type BitDropGreaterThanLength = BitDrop<'11111', BaseVector>
const bitdrop_test_4: true = {} as Equal<BitDropGreaterThanLength, []>

// BitTake
type BitTakeFirst = BitTake<'001', BaseVector>
const bittake_test_0: true = {} as Equal<BitTakeFirst, [0]>

type BitTakeNone = BitTake<'000', BaseVector>
const bittake_test_1: true = {} as Equal<BitTakeNone, []>

type BitTakeLessThanZero = BitTake<'1000000000000001', BaseVector>
const bittake_test_2: true = {} as Equal<BitTakeLessThanZero, []>

type BitTakeLength = BitTake<'010', BaseVector>
const bittake_test_3: true = {} as Equal<BitTakeLength, [0, 1]>

type BitTakeGreaterThanLength = BitTake<'11111', BaseVector>
const bittake_test_4: true = {} as Equal<BitTakeGreaterThanLength, BaseVector>

// BitInter
type BitInterFirst = BitInter<BaseVector, '001', '010'>
const bitinter_test_0: true = {} as Equal<BitInterFirst, [1, 2]>

type BitInterNone = BitInter<BaseVector, '001', '001'>
const bitinter_test_1: true = {} as Equal<BitInterNone, [1]>

type BitInterLessThanZero = BitInter<BaseVector, '1000000000000001', '1111111111111111'>
const bitinter_test_2: true = {} as Equal<BitInterLessThanZero, []>

type BitInterLength = BitInter<BaseVector, '000', '010'>
const bitinter_test_3: true = {} as Equal<BitInterLength, [0, 1, 2]>

type BitInterGreaterThanLength = BitInter<BaseVector, '000', '11111'>
const bitinter_test_4: true = {} as Equal<BitInterGreaterThanLength, BaseVector>

// BitRepeat
const bitrepeat_test_0: true = {} as Equal<BitRepeat<'0000000000000011', ['2']>, [['2'], ['2'], ['2']]>
const bitrepeat_test_1: true = {} as Equal<BitRepeat<'0000000000000000', ['2']>, []>

// Last
type LastTest = Last<[0, 1, 2]>
const lasttest_0: true = {} as Equal<LastTest, 2>
type LastEmpty = Last<[]>
const lasttest_1: true = {} as Equal<LastEmpty, never>
