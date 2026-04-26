import type { Equal } from '../../src/util.js'
import type { BitAbs, Neg1, BitOne } from '../../src/bit/index.js'

type Bit0 = "0000000000000000"
type Bit1 = "0000000000000001"
type BitMax = "0111111111111111"
type BitMin = "1000000000000000"
type BitNegMax = "1000000000000001"

// Existing Tests
const expected_7: true = {} as Equal<BitAbs<"00111">, "00111">
const expected_0: true = {} as Equal<BitAbs<"00000">, "00000">
const expected_1: true = {} as Equal<BitAbs<Neg1>, BitOne>

// Positive boundary
type Actual_Abs_Max = BitAbs<BitMax>
const expected_abs_max: true = {} as Equal<BitMax, Actual_Abs_Max>

// Negative near-boundary (-32767)
type Actual_Abs_NegMax = BitAbs<BitNegMax>
const expected_abs_neg_max: true = {} as Equal<BitMax, Actual_Abs_NegMax>

// Absolute Min (-32768) 
// Note: Depending on your implementation, this may overflow back to BitMin
type Actual_Abs_Min = BitAbs<BitMin>
const expected_abs_min: true = {} as Equal<BitMin, Actual_Abs_Min>

// Zero variants
type Actual_Abs_Zero = BitAbs<Bit0>
const expected_abs_zero: true = {} as Equal<Bit0, Actual_Abs_Zero>

type Actual_Abs_Neg2 = BitAbs<"1111111111111110">
const expected_abs_neg2: true = {} as Equal<"0000000000000010", Actual_Abs_Neg2>

type Actual_Abs_Neg1_Short = BitAbs<"1111111111111111">
const expected_abs_neg1_short: true = {} as Equal<"0000000000000001", Actual_Abs_Neg1_Short>

type Actual_Abs_Alt = BitAbs<"1111111111110101">
const expected_abs_alt: true = {} as Equal<"0000000000001011", Actual_Abs_Alt>
