import type { Equal } from '../../src/util.js'
import type { BitCut } from '../../src/bit/index.js'
import type * as Peano from "../../src/peano.js";

type T1 = [null]
type T2 = [[null]]
type T3 = [[[null]]]
type T5 = [[[[[null]]]]]

const bitcut0: true = {} as Equal<BitCut<"11111", Peano.T0>, "11111">
const bitcut1: true = {} as Equal<BitCut<"11111", T1>, "1111">

// Cut 2 bits
type Actual_Cut2 = BitCut<"10101", T2>
const test_cut2: true = {} as Equal<Actual_Cut2, "101">

// Cut 3 bits (with zeros)
type Actual_Cut3 = BitCut<"00011", T3>
const test_cut3: true = {} as Equal<Actual_Cut3, "11">

// Cut all bits exactly
type Actual_Cut_All = BitCut<"111", T3>
const test_cut_all: true = {} as Equal<Actual_Cut_All, "">

// Cut more than available bits (B extends `${infer _H}${infer T}` will fail)
type Actual_Cut_Over = BitCut<"11", T3>
const test_cut_over: true = {} as Equal<Actual_Cut_Over, never>

// Cut from empty string
type Actual_Cut_Empty = BitCut<"", T1>
const test_cut_empty: true = {} as Equal<Actual_Cut_Empty, never>

// Cut 0 from empty string (P extends T0 hits first)
type Actual_Cut0_Empty = BitCut<"", Peano.T0>
const test_cut0_empty: true = {} as Equal<Actual_Cut0_Empty, "">

// Boundary: 16-bit BitMax
type BitMax = "0111111111111111"
type Actual_Cut_Max_T5 = BitCut<BitMax, T5>
const test_cut_max_t5: true = {} as Equal<Actual_Cut_Max_T5, "11111111111">

// Ensuring specific bits remain
type Actual_Cut_Pattern = BitCut<"11001010", [[[[null]]]]>
const test_cut_pattern: true = {} as Equal<BitCut<"11001010", [[[[null]]]]>, "1010">
