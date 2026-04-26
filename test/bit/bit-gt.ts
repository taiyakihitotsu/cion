import type { Equal } from '../../src/util.js'
import type { BitGT } from '../../src/bit/index.js'

// --- Utility Constants for Boundaries ---
type Bit0 = '0000000000000000'
type Bit1 = '0000000000000001'
type BitNeg1 = '1111111111111111'
type BitMax = '0111111111111111' // 32767
type BitMin = '1000000000000000' // -32768

// --- Existing Tests (Short Bitstrings) ---
const bitsub0gt: true = {} as Equal<BitGT<"00111", "00101">, true>
const bitsub1gt: true = {} as Equal<BitGT<"00110", "00001">, true>
const bitsub2gt: true = {} as Equal<BitGT<"00000", "00000">, false>
const bitsub3gt: true = {} as Equal<BitGT<"11111", "11111">, false>
const bitsub4gt: true = {} as Equal<BitGT<"00111", "01000">, false>
const bitsub5gt: true = {} as Equal<BitGT<"0000000000000000", "1111111111111111">, true>

// --- Additional Boundary & Overflow Tests (16-bit) ---

// Max vs Min (The specific case where simple subtraction fails)
const test_gt_max_min: true = {} as Equal<BitGT<BitMax, BitMin>, true>
const test_gt_min_max: true = {} as Equal<BitGT<BitMin, BitMax>, false>

// Zero vs Positive/Negative
const test_gt_zero_neg: true = {} as Equal<BitGT<Bit0, BitNeg1>, true>
const test_gt_zero_pos: true = {} as Equal<BitGT<Bit0, Bit1>, false>

// Smallest steps around boundaries
const test_gt_near_min: true = {} as Equal<BitGT<'1000000000000001', BitMin>, true>
const test_gt_near_max: true = {} as Equal<BitGT<BitMax, '0111111111111110'>, true>

// Negative vs Negative
const test_gt_neg_comp: true = {} as Equal<BitGT<BitNeg1, '1111111111111110'>, true> // -1 >= -2
const test_gt_neg_min: true = {} as Equal<BitGT<BitMin, BitMin>, false>

// --- Mixed Bit Lengths (If supported by your implementation) ---
const test_gt_mixed_pos: true = {} as Equal<BitGT<"01", "0001">, true>
const test_gt_mixed_neg: true = {} as Equal<BitGT<"111111111111111111", "0000000000000001">, false>
