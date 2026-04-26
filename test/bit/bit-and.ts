import type { Equal } from '../../src/util.js'
import type { BitAnd } from '../../src/bit/index.js'

// Basic Patterns (Single Bit)
const bitand1: true = {} as Equal<BitAnd<`1`, `1`>, `1`>
const bitand2: true = {} as Equal<BitAnd<`1`, `0`>, `0`>
const bitand3: true = {} as Equal<BitAnd<`0`, `1`>, `0`>
const bitand4: true = {} as Equal<BitAnd<`0`, `0`>, `0`>

// Multi-bit Patterns
const bitand5: true = {} as Equal<BitAnd<`010`, `000`>, `000`>
const bitand6: true = {} as Equal<BitAnd<`111`, `111`>, `111`>
const bitand7: true = {} as Equal<BitAnd<`110`, `110`>, `110`>
const bitand8: true = {} as Equal<BitAnd<`000`, `000`>, `000`>

// Interleaved bits (1010 & 0101 = 0000)
type Actual_And_Interleaved = BitAnd<"1010", "0101">
const test_and_interleaved: true = {} as Equal<Actual_And_Interleaved, "0000">

// Partial match (1101 & 1011 = 1001)
type Actual_And_Partial = BitAnd<"1101", "1011">
const test_and_partial: true = {} as Equal<Actual_And_Partial, "1001">

// Different lengths (assuming sign extension or zero padding to 16-bit or longest)
type Actual_And_DiffLen = BitAnd<"111", "1">
const test_and_difflen: true = {} as Equal<Actual_And_DiffLen, "1">

// All ones with leading zeros
type Actual_And_Leading = BitAnd<"0011", "1111">
const test_and_leading: true = {} as Equal<Actual_And_Leading, "0011">

// Boundary: 16-bit Max & 16-bit Min
// 0111...1111 & 1000...0000 = 0000...0000
type BitMax = "0111111111111111"
type BitMin = "1000000000000000"
type Actual_And_Boundary = BitAnd<BitMax, BitMin>
const test_and_boundary: true = {} as Equal<Actual_And_Boundary, "0000000000000000">

// Boundary: -1 & Any value (X & -1 = X)
// "1111111111111111" & "0101..." = "0101..."
type BitNeg1 = "1111111111111111"
type Actual_And_Neg1 = BitAnd<BitNeg1, "0101010101010101">
const test_and_neg1: true = {} as Equal<Actual_And_Neg1, "0101010101010101">
