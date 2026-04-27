import type { Equal } from '../../src/util.js'
import type { BitShiftLeftOne, BitShiftLeft } from '../../src/bit/index.js'

type T0 = null
type T1 = [null]
type T2 = [[null]]
type T3 = [[[null]]]
type T4 = [[[[null]]]]

type Actual_S1_basic = BitShiftLeftOne<"101">
const expected_S1_basic: true = {} as Equal<"010", Actual_S1_basic>

type Actual_S1_overflow = BitShiftLeftOne<"111">
const expected_S1_overflow: true = {} as Equal<"110", Actual_S1_overflow>

type Actual_S1_zeros = BitShiftLeftOne<"000">
const expected_S1_zeros: true = {} as Equal<"000", Actual_S1_zeros>

type Actual_S1_empty = BitShiftLeftOne<"">
const expected_S1_empty: true = {} as Equal<"", Actual_S1_empty>

type Actual_S1_single = BitShiftLeftOne<"1">
const expected_S1_single: true = {} as Equal<"0", Actual_S1_single>

// Shift by 0
type Actual_L_T0 = BitShiftLeft<"1011", T0>
const expected_L_T0: true = {} as Equal<"1011", Actual_L_T0>

// Shift by 1
type Actual_L_T1 = BitShiftLeft<"1011", T1>
const expected_L_T1: true = {} as Equal<"0110", Actual_L_T1>

// Shift by 2
type Actual_L_T2 = BitShiftLeft<"1011", T2>
const expected_L_T2: true = {} as Equal<"1100", Actual_L_T2>

// Shift by 3
type Actual_L_T3 = BitShiftLeft<"1011", T3>
const expected_L_T3: true = {} as Equal<"1000", Actual_L_T3>

// Shift by 4 (All bits shifted out)
type Actual_L_T4 = BitShiftLeft<"1011", T4>
const expected_L_T4: true = {} as Equal<"0000", Actual_L_T4>

// Boundary: Max Positive (16-bit)
type BitMax = '0111111111111111'
type Actual_L_Max_T1 = BitShiftLeft<BitMax, T1>
const expected_L_Max_T1: true = {} as Equal<'1111111111111110', Actual_L_Max_T1>

// Boundary: Min Negative (16-bit)
type BitMin = '1000000000000000'
type Actual_L_Min_T1 = BitShiftLeft<BitMin, T1>
const expected_L_Min_T1: true = {} as Equal<'0000000000000000', Actual_L_Min_T1>

// Pattern: Alternating bits
type Actual_L_Alt = BitShiftLeft<"1010", T2>
const expected_L_Alt: true = {} as Equal<"1000", Actual_L_Alt>
