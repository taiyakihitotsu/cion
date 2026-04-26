import type { Equal } from '../../src/util.js'
import type { BitLT } from '../../src/bit/index.js'

// --- Utility Constants ---
type Bit0 = '0000000000000000'
type Bit1 = '0000000000000001'
type BitNeg1 = '1111111111111111'
type BitMax = '0111111111111111'
type BitMin = '1000000000000000'

// --- Existing Tests ---
const bitsub0lt: true = {} as Equal<BitLT<"00111", "00101">, false>
const bitsub1lt: true = {} as Equal<BitLT<"00110", "00001">, false>
const bitsub2lt: true = {} as Equal<BitLT<"00000", "00000">, false>
const bitsub3lt: true = {} as Equal<BitLT<"11111", "11111">, false>
const bitsub4lt: true = {} as Equal<BitLT<"00111", "01000">, true>
const bitsub5lt: true = {} as Equal<BitLT<"0000000000000000", "1111111111111111">, false>

// --- Additional Boundary & Overflow Tests (16-bit) ---

// Min vs Max (Overflow risk)
const test_lt_min_max: true = {} as Equal<BitLT<BitMin, BitMax>, true>
const test_lt_max_min: true = {} as Equal<BitLT<BitMax, BitMin>, false>

// Zero and Sign boundaries
const test_lt_neg_zero: true = {} as Equal<BitLT<BitNeg1, Bit0>, true>
const test_lt_pos_zero: true = {} as Equal<BitLT<Bit1, Bit0>, false>

// Smallest steps
const test_lt_min_near: true = {} as Equal<BitLT<BitMin, '1000000000000001'>, true>
const test_lt_max_near: true = {} as Equal<BitLT<'0111111111111110', BitMax>, true>

// Negative vs Negative
const test_lt_neg_neg: true = {} as Equal<BitLT<'1111111111111110', BitNeg1>, true> // -2 < -1
const test_lt_min_self: true = {} as Equal<BitLT<BitMin, BitMin>, false>

// Maximum Negative vs Positive 1
const test_lt_min_one: true = {} as Equal<BitLT<BitMin, Bit1>, true>
