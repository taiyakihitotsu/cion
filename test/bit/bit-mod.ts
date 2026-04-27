import type {Equal} from '../../src/util.js'
import type {BitMod} from '../../src/bit/index.js'
import {CurPad} from '../../src/bit/index.js'
import type { TNil } from '../../src/sexprtypes.js'

const testbitmod0: true = {} as Equal<BitMod<"00001001", "00000001">,  `${typeof CurPad}00000000`>
const testbitmod1: true = {} as Equal<BitMod<"00001001", "00000011">,  `${typeof CurPad}00000000`>
const testbitmod2: true = {} as Equal<BitMod<"00001001", "00000010">,  `${typeof CurPad}00000001`>
const testbitmod3: true = {} as Equal<BitMod<"00001001", "00000000">, TNil>
const testbitmod4: true = {} as Equal<BitMod<"00000010", "00001010">,  `${typeof CurPad}00000010`>

// > https://clojuredocs.org/clojure.core/mod
// 1. (mod 10 3) => 1
type TestMod_PP = BitMod<"0000000000001010", "0000000000000011">
const test_pp: true = {} as Equal<TestMod_PP, "0000000000000001">

// 2. (mod -10 3) => 2
type TestMod_NP = BitMod<"1111111111110110", "0000000000000011">
const test_np: true = {} as Equal<TestMod_NP, "0000000000000010">

// 3. (mod 10 -3) => -2
type TestMod_PN = BitMod<"0000000000001010", "1111111111111101">
const test_pn: true = {} as Equal<TestMod_PN, "1111111111111110">

// 4. (mod -10 -3) => -1
type TestMod_NN = BitMod<"1111111111110110", "1111111111111101">
const test_nn: true = {} as Equal<TestMod_NN, "1111111111111111">

// 5. (mod 0 7) => 0
type TestMod_Zero = BitMod<"0000000000000000", "0000000000000111">
const test_zero: true = {} as Equal<TestMod_Zero, "0000000000000000">

// 6. testbitmod5: (mod -2 -6) => -2
type TestMod5_Clojure = BitMod<"1111111111111110", "1111111111111010">
const test_mod5_fixed: true = {} as Equal<TestMod5_Clojure, "1111111111111110">

type Actual_Mod_Same = BitMod<"00001100", "00001100">
const testbitmod_same: true = {} as Equal<Actual_Mod_Same, `${typeof CurPad}00000000`>

// (mod -5 3) = 1
type Actual_Mod_Neg_Dividend = BitMod<"1111111111111011", "0000000000000011">
const testbitmod_neg_dividend: true = {} as Equal<Actual_Mod_Neg_Dividend, "0000000000000001">

// (mod 11 -3) = -1
type Actual_Mod_Neg_Divisor = BitMod<"0000000000001011", "1111111111111101">
const testbitmod_neg_divisor: true = {} as Equal<Actual_Mod_Neg_Divisor, "1111111111111111">

type Actual_Mod_Both_Neg = BitMod<"1111111111111011", "1111111111111101">
const testbitmod_both_neg: true = {} as Equal<Actual_Mod_Both_Neg, "1111111111111110">

type Actual_Mod_Large_Divisor = BitMod<"00000101", "0111111111111111">
const testbitmod_large_divisor: true = {} as Equal<Actual_Mod_Large_Divisor, `${typeof CurPad}00000101`>

type Actual_Mod_Zero_Dividend = BitMod<"0000000000000000", "0000000000000111">
const testbitmod_zero_dividend: true = {} as Equal<Actual_Mod_Zero_Dividend, "0000000000000000">

type Actual_Mod_By_One = BitMod<"0101010101010101", "0000000000000001">
const testbitmod_by_one: true = {} as Equal<Actual_Mod_By_One, "0000000000000000">
