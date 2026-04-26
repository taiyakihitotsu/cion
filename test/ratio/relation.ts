import type { Equal } from '../../src/util.js'
import type { Relation } from '../../src/ratio/relation.js'

// --- Utility Constants (for readability in tests) ---
type Bit0 = '0000000000000000'
type Bit1 = '0000000000000001'
type Bit2 = '0000000000000010'
type Bit3 = '0000000000000011'
type BitNeg1 = '1111111111111111' // 2's complement assumed
type BitMax = '0111111111111111' // Max Positive
type BitMin = '1000000000000000' // Min Negative

// --- Integer Tests (1-member tuple) ---

// Standard comparison
type Test_Int_Pos = Relation<Bit3, Bit2, '>'>
const test_int_pos: true = {} as Equal<Test_Int_Pos, true>

// Zero and Negatives
type Test_Int_Zero_Eq = Relation<Bit0, Bit0, '='>
const test_int_zero_eq: true = {} as Equal<Test_Int_Zero_Eq, true>

type Test_Int_Neg_LT = Relation<BitNeg1, Bit0, '<'>
const test_int_neg_lt: true = {} as Equal<Test_Int_Neg_LT, true>

// Boundary Cases
type Test_Int_Max_MinGT = Relation<BitMax, BitMin, '>'>
const test_int_max_minGT: true = {} as Equal<Test_Int_Max_MinGT, true>

type Test_Int_Max_MinLT = Relation<BitMax, BitMin, '<'>
const test_int_max_minLT: true = {} as Equal<Test_Int_Max_MinLT, false>

type Test_Int_Max_MinGTE = Relation<BitMax, BitMin, '>'>
const test_int_max_minGTE: true = {} as Equal<Test_Int_Max_MinGTE, true>

type Test_Int_Max_MinLTE = Relation<BitMax, BitMin, '<'>
const test_int_max_minLTE: true = {} as Equal<Test_Int_Max_MinLTE, false>


// --- Ratio Tests (2-member tuple) ---

// 3/2 > 2/3
type Test_Ratio_Cross = Relation<[Bit3, Bit2], [Bit2, Bit3], '>'>
const test_ratio_cross: true = {} as Equal<Test_Ratio_Cross, true>

// 1/1 = 2/2
type Test_Ratio_Equivalent = Relation<[Bit1, Bit1], [Bit2, Bit2], '='>
const test_ratio_equivalent: true = {} as Equal<Test_Ratio_Equivalent, true>

// Negative ratios: -1/2 < 1/2
type Test_Ratio_Neg_LT = Relation<[BitNeg1, Bit2], [Bit1, Bit2], '<'>
const test_ratio_neg_lt: true = {} as Equal<Test_Ratio_Neg_LT, true>

// -3/2 < -1/2
type Test_Ratio_Both_Neg = Relation<['1111111111111101', Bit2], [BitNeg1, Bit2], '<'>
const test_ratio_both_neg: true = {} as Equal<Test_Ratio_Both_Neg, true>


// --- Mixed Comparison Tests ---
// Integer vs Ratio (Structure: [bit] vs [bit, bit])

type Test_Mixed_GT = Relation<Bit2, [Bit1, Bit2], '>'>
const test_mixed_gt: true = {} as Equal<Test_Mixed_GT, never>

type Test_Mixed_LT = Relation<BitNeg1, [BitNeg1, Bit2], '<'>
const test_mixed_lt: true = {} as Equal<Test_Mixed_LT, never>

type Test_Mixed_Eq = Relation<Bit1, [Bit1, Bit1], '='>
const test_mixed_eq: true = {} as Equal<Test_Mixed_Eq, never>


// --- Edge / Extreme Cases ---

type Test_Ratio_Max = Relation<[BitMax, Bit1], BitMax, '='>
const test_ratio_max: true = {} as Equal<Test_Ratio_Max, never>

type Test_Ratio_Tiny = Relation<[Bit1, BitMax], Bit0, '>'>
const test_ratio_tiny: true = {} as Equal<Test_Ratio_Tiny, never>


// --- Additional Integer Tests (string) ---

// Large positive comparison
type Test_Int_Large_GT = Relation<'0100000000000000', '0011111111111111', '>'>
const test_int_large_gt: true = {} as Equal<Test_Int_Large_GT, true>

// Negative vs Negative (-2 < -1)
type Test_Int_Neg_Neg_LT = Relation<'1111111111111110', BitNeg1, '<'>
const test_int_neg_neg_lt: true = {} as Equal<Test_Int_Neg_Neg_LT, true>

// Min negative vs -1
type Test_Int_Min_Neg1_LT = Relation<BitMin, BitNeg1, '<'>
const test_int_min_neg1_lt: true = {} as Equal<Test_Int_Min_Neg1_LT, true>

// Just below Max
type Test_Int_NearMax = Relation<'0111111111111110', BitMax, '<'>
const test_int_near_max: true = {} as Equal<Test_Int_NearMax, true>


// --- Additional Ratio Tests ([string, string]) ---

// Same numerator, different denominator (1/2 > 1/3)
type Test_Ratio_SameNum = Relation<[Bit1, Bit2], [Bit1, Bit3], '>'>
const test_ratio_same_num: true = {} as Equal<Test_Ratio_SameNum, true>

// Same denominator, different numerator (2/3 > 1/3)
type Test_Ratio_SameDen = Relation<[Bit2, Bit3], [Bit1, Bit3], '>'>
const test_ratio_same_den: true = {} as Equal<Test_Ratio_SameDen, true>

// Negative denominator handling (if supported: 1/-1 = -1/1)
// Note: If your logic normalizes sign to numerator, this is a good test.
type Test_Ratio_Neg_Den = Relation<[Bit1, BitNeg1], [BitNeg1, Bit1], '='>
const test_ratio_neg_den: true = {} as Equal<Test_Ratio_Neg_Den, true>

// Comparison near 0 (1/Max > 0/1)
type Test_Ratio_NearZero = Relation<[Bit1, BitMax], [Bit0, Bit1], '>'>
const test_ratio_near_zero: true = {} as Equal<Test_Ratio_NearZero, true>

// Large Numerators (Max/2 > 1/2)
type Test_Ratio_LargeNum = Relation<['0011111111111111', Bit2], [Bit1, Bit2], '>'>
const test_ratio_large_num: true = {} as Equal<Test_Ratio_LargeNum, true>


// --- Precise Boundary / Overflow Check ---

// (Max/Bit2) vs (Max/Bit3) -> common denominator might overflow if not handled
// This tests the Scaling logic's robustness
type Test_Ratio_Scaling_Overflow = Relation<[BitMax, Bit2], [BitMax, Bit3], '>'>
const test_ratio_scaling_overflow: true = {} as Equal<Test_Ratio_Scaling_Overflow, true>

// Min/1 is the smallest possible ratio
type Test_Ratio_AbsoluteMin = Relation<[BitMin, Bit1], [BitNeg1, Bit1], '<'>
const test_ratio_absolute_min: true = {} as Equal<Test_Ratio_AbsoluteMin, true>


// --- Strict Type Consistency (Never cases) ---

// Swapped types should still be never
type Test_Mixed_Inverted = Relation<[Bit1, Bit1], Bit1, '='>
const test_mixed_inverted: true = {} as Equal<Test_Mixed_Inverted, never>

// Empty or invalid structures
type Test_Invalid_Empty = Relation<
  // @ts-expect-error: 
  [],
  [], '='>
const test_invalid_empty: true = {} as Equal<Test_Invalid_Empty, never>

// --- Exhaustive Comparison Matrix ---

// 1. Integer: [BitMax] vs [BitMin] (Crucial for overflow check)
type Test_Int_Matrix_MaxMin_GT  = Relation<BitMax, BitMin, '>'>
type Test_Int_Matrix_MaxMin_LT  = Relation<BitMax, BitMin, '<'>
type Test_Int_Matrix_MaxMin_GTE = Relation<BitMax, BitMin, '>='>
type Test_Int_Matrix_MaxMin_LTE = Relation<BitMax, BitMin, '<='>
type Test_Int_Matrix_MaxMin_EQ  = Relation<BitMax, BitMin, '='>

const test_int_maxmin_matrix: true = {} as 
  Equal<Test_Int_Matrix_MaxMin_GT,  true>  &
  Equal<Test_Int_Matrix_MaxMin_LT,  false> &
  Equal<Test_Int_Matrix_MaxMin_GTE, true>  &
  Equal<Test_Int_Matrix_MaxMin_LTE, false> &
  Equal<Test_Int_Matrix_MaxMin_EQ,  false>

// 2. Integer: Same values [BitNeg1] vs [BitNeg1]
type Test_Int_Matrix_NegEq_GT  = Relation<BitNeg1, BitNeg1, '>'>
type Test_Int_Matrix_NegEq_LT  = Relation<BitNeg1, BitNeg1, '<'>
type Test_Int_Matrix_NegEq_GTE = Relation<BitNeg1, BitNeg1, '>='>
type Test_Int_Matrix_NegEq_LTE = Relation<BitNeg1, BitNeg1, '<='>
type Test_Int_Matrix_NegEq_EQ  = Relation<BitNeg1, BitNeg1, '='>

const test_int_negeq_matrix: true = {} as 
  Equal<Test_Int_Matrix_NegEq_GT,  false> &
  Equal<Test_Int_Matrix_NegEq_LT,  false> &
  Equal<Test_Int_Matrix_NegEq_GTE, true>  &
  Equal<Test_Int_Matrix_NegEq_LTE, true>  &
  Equal<Test_Int_Matrix_NegEq_EQ,  true>

// 3. Ratio: [3/2] vs [2/3] (Fractional Cross-multiplication check)
type Test_Ratio_Matrix_Cross_GT  = Relation<[Bit3, Bit2], [Bit2, Bit3], '>'>
type Test_Ratio_Matrix_Cross_LT  = Relation<[Bit3, Bit2], [Bit2, Bit3], '<'>
type Test_Ratio_Matrix_Cross_GTE = Relation<[Bit3, Bit2], [Bit2, Bit3], '>='>
type Test_Ratio_Matrix_Cross_LTE = Relation<[Bit3, Bit2], [Bit2, Bit3], '<='>
type Test_Ratio_Matrix_Cross_EQ  = Relation<[Bit3, Bit2], [Bit2, Bit3], '='>

const test_ratio_cross_matrix: true = {} as 
  Equal<Test_Ratio_Matrix_Cross_GT,  true>  &
  Equal<Test_Ratio_Matrix_Cross_LT,  false> &
  Equal<Test_Ratio_Matrix_Cross_GTE, true>  &
  Equal<Test_Ratio_Matrix_Cross_LTE, false> &
  Equal<Test_Ratio_Matrix_Cross_EQ,  false>

// 4. Ratio: Same value with different forms [1/1] vs [2/2]
type Test_Ratio_Matrix_Equiv_GT  = Relation<[Bit1, Bit1], [Bit2, Bit2], '>'>
type Test_Ratio_Matrix_Equiv_LT  = Relation<[Bit1, Bit1], [Bit2, Bit2], '<'>
type Test_Ratio_Matrix_Equiv_GTE = Relation<[Bit1, Bit1], [Bit2, Bit2], '>='>
type Test_Ratio_Matrix_Equiv_LTE = Relation<[Bit1, Bit1], [Bit2, Bit2], '<='>
type Test_Ratio_Matrix_Equiv_EQ  = Relation<[Bit1, Bit1], [Bit2, Bit2], '='>

const test_ratio_equiv_matrix: true = {} as 
  Equal<Test_Ratio_Matrix_Equiv_GT,  false> &
  Equal<Test_Ratio_Matrix_Equiv_LT,  false> &
  Equal<Test_Ratio_Matrix_Equiv_GTE, true>  &
  Equal<Test_Ratio_Matrix_Equiv_LTE, true>  &
  Equal<Test_Ratio_Matrix_Equiv_EQ,  true>

// 5. Ratio: Negative [ -1/2 ] vs [ -3/2 ]
type Test_Ratio_Matrix_Neg_GT  = Relation<[BitNeg1, Bit2], ['1111111111111101', Bit2], '>'>
type Test_Ratio_Matrix_Neg_LT  = Relation<[BitNeg1, Bit2], ['1111111111111101', Bit2], '<'>
type Test_Ratio_Matrix_Neg_GTE = Relation<[BitNeg1, Bit2], ['1111111111111101', Bit2], '>='>
type Test_Ratio_Matrix_Neg_LTE = Relation<[BitNeg1, Bit2], ['1111111111111101', Bit2], '<='>
type Test_Ratio_Matrix_Neg_EQ  = Relation<[BitNeg1, Bit2], ['1111111111111101', Bit2], '='>

const test_ratio_neg_matrix: true = {} as 
  Equal<Test_Ratio_Matrix_Neg_GT,  true>  &
  Equal<Test_Ratio_Matrix_Neg_LT,  false> &
  Equal<Test_Ratio_Matrix_Neg_GTE, true>  &
  Equal<Test_Ratio_Matrix_Neg_LTE, false> &
  Equal<Test_Ratio_Matrix_Neg_EQ,  false>

// 6. Extreme: Min Negative Ratio vs Zero [ BitMin/Bit1 ] vs [ Bit0/Bit1 ]
type Test_Ratio_Matrix_MinZero_GT  = Relation<[BitMin, Bit1], [Bit0, Bit1], '>'>
type Test_Ratio_Matrix_MinZero_LT  = Relation<[BitMin, Bit1], [Bit0, Bit1], '<'>
type Test_Ratio_Matrix_MinZero_GTE = Relation<[BitMin, Bit1], [Bit0, Bit1], '>='>
type Test_Ratio_Matrix_MinZero_LTE = Relation<[BitMin, Bit1], [Bit0, Bit1], '<='>
type Test_Ratio_Matrix_MinZero_EQ  = Relation<[BitMin, Bit1], [Bit0, Bit1], '='>

const test_ratio_minzero_matrix: true = {} as 
  Equal<Test_Ratio_Matrix_MinZero_GT,  false> &
  Equal<Test_Ratio_Matrix_MinZero_LT,  true>  &
  Equal<Test_Ratio_Matrix_MinZero_GTE, false> &
  Equal<Test_Ratio_Matrix_MinZero_LTE, true>  &
  Equal<Test_Ratio_Matrix_MinZero_EQ,  false>
