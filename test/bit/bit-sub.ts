import type {Equal} from '../../src/util.js'
import type {BitSub } from '../../src/bit/index.js'
import {CurPad} from '../../src/bit/index.js'

const bitsub0: true = {} as Equal<BitSub<"00111", "00101">,  `${typeof CurPad}00000010`>
const bitsub1: true = {} as Equal<BitSub<"00110", "00001">,  `${typeof CurPad}00000101`>
const bitsub2: true = {} as Equal<BitSub<"00000", "00000">,  `${typeof CurPad}00000000`>
const bitsub3: true = {} as Equal<BitSub<"11111", "11111">,  `${typeof CurPad}00000000`>
const bitsub4: true = {} as Equal<BitSub<"00111", "01000">, "1111111111111111">
const bitsub5: true = {} as Equal<BitSub<"00000", "11111">, "1111111111100001">
const bitsub6: true = {} as Equal<BitSub<"1111111111111111","0000000000000001">, '1111111111111110'>

type Actual_Sub_Neg_Neg = BitSub<"1111111111111110", "1111111111111111">
const testbitsub_neg_neg: true = {} as Equal<Actual_Sub_Neg_Neg, "1111111111111111">

type Actual_Sub_Wrap_Around = BitSub<"1000000000000000", "0000000000000001">
const testbitsub_wrap: true = {} as Equal<Actual_Sub_Wrap_Around, "0111111111111111">

type Actual_Sub_MaxPos_NegOne = BitSub<"0111111111111111", "1111111111111111">
const testbitsub_max_neg: true = {} as Equal<Actual_Sub_MaxPos_NegOne, "1000000000000000">

type Actual_Sub_Borrow_Chain = BitSub<"00010000", "00000001">
const testbitsub_borrow: true = {} as Equal<Actual_Sub_Borrow_Chain, `${typeof CurPad}00001111`>

type Actual_Sub_Zero_Minus_Pos = BitSub<"0000", "0001">
const testbitsub_zero_pos: true = {} as Equal<Actual_Sub_Zero_Minus_Pos, "1111111111111111">

type Actual_Sub_Same_Long = BitSub<"1010101010101010", "1010101010101010">
const testbitsub_same_long: true = {} as Equal<Actual_Sub_Same_Long, "0000000000000000">

type Actual_Sub_Short_Operands = BitSub<"1", "1">
const testbitsub_short: true = {} as Equal<Actual_Sub_Short_Operands, "0000000000000000">
