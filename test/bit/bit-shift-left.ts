import type { Equal } from '../../src/util.js'
import type { BitShiftLeft } from '../../src/bit/index.js'

type T0 = null
type T1 = [null]
type T2 = [[null]]
type T3 = [[[null]]]
type T4 = [[[[null]]]]

// Shift by 0
type Actual_id = BitShiftLeft<"101", T0>
const expected: true = {} as Equal<"101", Actual_id>

// Shift by 1
type Actual_4digits_1010 = BitShiftLeft<"0101", T1>
const expected_1010: true = {} as Equal<"1010", Actual_4digits_1010>

type Actual_3digits_010 = BitShiftLeft<"101", T1>
const expected_010: true = {} as Equal<"010", Actual_3digits_010>

// Shift by 2
type Actual_5digits_100 = BitShiftLeft<"00101", T2>
const expected_5digits_100: true = {} as Equal<"10100", Actual_5digits_100>

type Actual_3digits_100 = BitShiftLeft<"101", T2>
const expected_3digits_100: true = {} as Equal<"100", Actual_3digits_100>

// Shift with all zeros
type Actual_shift_zeros = BitShiftLeft<"000", T2>
const bitsl_zeros: true = {} as Equal<"000", Actual_shift_zeros>

// Empty string shift (depends on BitShiftLeftOne implementation, usually adds "0")
type Actual_shift_empty = BitShiftLeft<"", T1>
const bitsl_empty_one: true = {} as Equal<"", Actual_shift_empty>
