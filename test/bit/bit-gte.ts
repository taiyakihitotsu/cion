import type { Equal } from '../../src/util.js'
import type { BitGTE } from '../../src/bit/index.js'

// --- Utility Constants for Boundaries ---
type Bit0 = '0000000000000000'
type Bit1 = '0000000000000001'
type BitNeg1 = '1111111111111111'
type BitMax = '0111111111111111' // 32767
type BitMin = '1000000000000000' // -32768

// --- Existing Tests (Short Bitstrings) ---
const bitsub0gte: true = {} as Equal<BitGTE<"00111", "00101">, true>
const bitsub1gte: true = {} as Equal<BitGTE<"00110", "00001">, true>
const bitsub2gte: true = {} as Equal<BitGTE<"00000", "00000">, true>
const bitsub3gte: true = {} as Equal<BitGTE<"11111", "11111">, true>
const bitsub4gte: true = {} as Equal<BitGTE<"00111", "01000">, false>
const bitsub5gte: true = {} as Equal<BitGTE<"0000000000000000", "1111111111111111">, true>

// --- Additional Boundary & Overflow Tests (16-bit) ---

// Max vs Min (The specific case where simple subtraction fails)
const test_gte_max_min: true = {} as Equal<BitGTE<BitMax, BitMin>, true>
const test_gte_min_max: true = {} as Equal<BitGTE<BitMin, BitMax>, false>

// Zero vs Positive/Negative
const test_gte_zero_neg: true = {} as Equal<BitGTE<Bit0, BitNeg1>, true>
const test_gte_zero_pos: true = {} as Equal<BitGTE<Bit0, Bit1>, false>

// Smallest steps around boundaries
const test_gte_near_min: true = {} as Equal<BitGTE<'1000000000000001', BitMin>, true>
const test_gte_near_max: true = {} as Equal<BitGTE<BitMax, '0111111111111110'>, true>

// Negative vs Negative
const test_gte_neg_comp: true = {} as Equal<BitGTE<BitNeg1, '1111111111111110'>, true> // -1 >= -2
const test_gte_neg_min: true = {} as Equal<BitGTE<BitMin, BitMin>, true>

// --- Mixed Bit Lengths (If supported by your implementation) ---
const test_gte_mixed_pos: true = {} as Equal<BitGTE<"01", "0001">, true>
const test_gte_mixed_neg: true = {} as Equal<BitGTE<"1111111111111111", "0000000000000001">, false>
