import type {Equal} from '../../src/util.js'
import type {BitShiftRight} from '../../src/bit/index.js'

const bitsr0: true = {} as Equal<BitShiftRight<"111111", [[[[[[null]]]]]], [[null]]>, "1111">
const bitsr1: true = {} as Equal<BitShiftRight<"111111", [[[[[[null]]]]]], [[[null]]]>, "111">

// Shift by 0
const bitsr_zero: true = {} as Equal<BitShiftRight<"101010", [[[[[[null]]]]]], null>, "101010">

// Shift by 1
const bitsr_one: true = {} as Equal<BitShiftRight<"101010", [[[[[[null]]]]]], [null]>, "10101">

// Shift all bits (Full shift)
type Actual_shift_all = BitShiftRight<"111", [[[null]]], [[[null]]]>
const bitsr_all: true = {} as Equal<"000", Actual_shift_all>

// Shift beyond length
type Actual_shift_beyond = BitShiftRight<"111", [[[null]]], [[[[null]]]]>
const bitsr_beyond: true = {} as Equal<"000", Actual_shift_beyond>

// --- Different Bit Patterns ---

// Right shift with zeros at the end
type Actual_shift_zero = BitShiftRight<"1100", [[[[null]]]], null>
const bitsr_zeros_end: true = {} as Equal<"1100", Actual_shift_zero>

// --- Large Bitstring / Peano Tests ---

type Peano4 = [[[[null]]]]
type Peano8 = [[[[[[[[null]]]]]]]]

const bitsr_large_bits: true = {} as Equal<
  BitShiftRight<"1010101010", [[[[[[[[[[null]]]]]]]]]], Peano4>, 
  "101010"
>

const bitsr_large_shift: true = {} as Equal<
  BitShiftRight<"1111111111", [[[[[[[[[[null]]]]]]]]]], Peano8>, 
  "11"
>

// --- Edge Cases ---

// Empty bitstring
// @ts-expect-error:
const bitsr_empty: true = {} as Equal<BitShiftRight<"", [], []>, "">

// Single bit string
const bitsr_single_to_empty: true = {} as Equal<BitShiftRight<"1", [[null]], [[null]]>, "00">
const bitsr_single_stay: true = {} as Equal<BitShiftRight<"1", [[null]], null>, "1">
