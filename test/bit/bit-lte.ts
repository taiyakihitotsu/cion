import type { Equal } from '../../src/util.js'
import type { BitLTE } from '../../src/bit/index.js'

// --- Utility Constants ---
type Bit0 = '0000000000000000'
type Bit1 = '0000000000000001'
type BitNeg1 = '1111111111111111'
type BitMax = '0111111111111111'
type BitMin = '1000000000000000'

// --- Existing Tests ---
const bitsub0lte: true = {} as Equal<BitLTE<"00111", "00101">, false>
const bitsub1lte: true = {} as Equal<BitLTE<"00110", "00001">, false>
const bitsub2lte: true = {} as Equal<BitLTE<"00000", "00000">, true>
const bitsub3lte: true = {} as Equal<BitLTE<"11111", "11111">, true>
const bitsub4lte: true = {} as Equal<BitLTE<"00111", "01000">, true>
const bitsub5lte: true = {} as Equal<BitLTE<"0000000000000000", "1111111111111111">, false>

// --- Additional Boundary & Overflow Tests (16-bit) ---

// Min vs Max (Overflow risk)
const test_lte_min_max: true = {} as Equal<BitLTE<BitMin, BitMax>, true>
const test_lte_max_min: true = {} as Equal<BitLTE<BitMax, BitMin>, false>

// Zero and Sign boundaries
const test_lte_neg_zero: true = {} as Equal<BitLTE<BitNeg1, Bit0>, true>
const test_lte_pos_zero: true = {} as Equal<BitLTE<Bit1, Bit0>, false>

// Smallest steps
const test_lte_min_near: true = {} as Equal<BitLTE<BitMin, '1000000000000001'>, true>
const test_lte_max_near: true = {} as Equal<BitLTE<'0111111111111110', BitMax>, true>

// Negative vs Negative
const test_lte_neg_neg: true = {} as Equal<BitLTE<'1111111111111110', BitNeg1>, true> // -2 < -1
const test_lte_min_self: true = {} as Equal<BitLTE<BitMin, BitMin>, true>

// Maximum Negative vs Positive 1
const test_lte_min_one: true = {} as Equal<BitLTE<BitMin, Bit1>, true>
