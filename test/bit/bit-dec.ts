import type { Equal } from '../../src/util.js'
import type { BitDec } from '../../src/bit/index.js'

const testbitdec0: true = {} as Equal<BitDec<"0000000000000001">, "0000000000000000">
const testbitdec1: true = {} as Equal<BitDec<"0000000000000000">, "1111111111111111">
const testbitdec2: true = {} as Equal<BitDec<"0000000000000100">, "0000000000000011">

// 1000 -> 0111 (Borrow propagates through multiple zeros)
type Actual_Dec_BorrowChain = BitDec<"01000">
const test_dec_borrow_chain: true = {} as Equal<Actual_Dec_BorrowChain, "0000000000000111">

// 1110 -> 1101 (Simple decrement of negative)
type Actual_Neg_Dec = BitDec<"11110">
const test_neg_dec: true = {} as Equal<Actual_Neg_Dec, "0000000000011101">

// Min Negative -> Max Positive (100...00 -> 011...11)
type BitMin = "1000000000000000"
type BitMax = "0111111111111111"
type Actual_Dec_Min = BitDec<BitMin>
const test_dec_min_wrap: true = {} as Equal<Actual_Dec_Min, BitMax>

// Positive near-boundary
type Actual_Dec_PosBoundary = BitDec<"0000000000001000">
const test_dec_pos_boundary: true = {} as Equal<Actual_Dec_PosBoundary, "0000000000000111">

// Negative near-boundary
type Actual_Dec_NegBoundary = BitDec<"1111111111111111">
const test_dec_neg_boundary: true = {} as Equal<Actual_Dec_NegBoundary, "1111111111111110">

type Actual_Dec_Short0 = BitDec<"00">
const test_dec_short0: true = {} as Equal<Actual_Dec_Short0, "1111111111111111">

type Actual_Dec_Short1 = BitDec<"10">
const test_dec_short1: true = {} as Equal<Actual_Dec_Short1, "0000000000000001">

type Actual_Dec_Alt = BitDec<"10101">
const test_dec_alt: true = {} as Equal<Actual_Dec_Alt, "0000000000010100">
