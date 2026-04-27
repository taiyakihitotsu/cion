import type {Equal} from '../../src/util.js'
import type {BitMul} from '../../src/bit/index.js'
import {CurPad} from '../../src/bit/index.js'

// test
// 7,5,35
// 6,2,12
// 0,0,0
// 31,1,31
// 1,1,1
// 0,1,0
// 1,0,0
const bitmul0: true = {} as Equal<BitMul<"00111", "00101">,  `${typeof CurPad}00100011`>
const bitmul1: true = {} as Equal<BitMul<"00110", "00010">,  `${typeof CurPad}00001100`>
const bitmul2: true = {} as Equal<BitMul<"00000", "00000">,  `${typeof CurPad}00000000`>
const bitmul3: true = {} as Equal<BitMul<"11111", "00001">,  `${typeof CurPad}00011111`>
const bitmul4: true = {} as Equal<BitMul<"00001", "00001">,  `${typeof CurPad}00000001`>
const bitmul5: true = {} as Equal<BitMul<"00000", "00001">,  `${typeof CurPad}00000000`>
const bitmul6: true = {} as Equal<BitMul<"00001", "00000">,  `${typeof CurPad}00000000`>
const bitmul7: true = {} as Equal<BitMul<'1111111111111111', '1111111111111111'>, `${typeof CurPad}00000001`>
const bitmul8: true = {} as Equal<BitMul<'1111111111111111', '1111111111110000'>, `${typeof CurPad}00010000`>
const bitmul9: true = {} as Equal<BitMul<'1111111111110000', '1111111111111111'>, `${typeof CurPad}00010000`>
const bitmul10: true = {} as Equal<BitMul<'0111111111110000', '1111111111111111'>, '1000000000010000'>
const bitmul11: true = {} as Equal<BitMul<'1111111111111111','0111111111110000'>, '1000000000010000'>

type Actual_Mul_Neg_Pos = BitMul<"1111111111111110", "0000000000000011">
const testbitmul_neg_pos: true = {} as Equal<Actual_Mul_Neg_Pos, "1111111111111010">

type Actual_Mul_Both_Neg = BitMul<"1111111111111110", "1111111111111110">
const testbitmul_both_neg: true = {} as Equal<Actual_Mul_Both_Neg, `${typeof CurPad}00000100`>

type Actual_Mul_Max_Zero = BitMul<"0111111111111111", "0000000000000000">
const testbitmul_max_zero: true = {} as Equal<Actual_Mul_Max_Zero, "0000000000000000">

type Actual_Mul_PowerOfTwo = BitMul<"0000000000000100", "0000000000000100">
const testbitmul_power_two: true = {} as Equal<Actual_Mul_PowerOfTwo, `${typeof CurPad}00010000`>

type Actual_Mul_Overflow_Boundary = BitMul<"0100000000000000", "0000000000000010">
const testbitmul_overflow: true = {} as Equal<Actual_Mul_Overflow_Boundary, "1000000000000000">

type Actual_Mul_Mixed_Small = BitMul<"011", "011">
const testbitmul_small_mixed: true = {} as Equal<Actual_Mul_Mixed_Small, `${typeof CurPad}00001001`>

type Actual_Mul_Short_Neg = BitMul<"1111111111111111", "0000000000000011">
const testbitmul_short_neg: true = {} as Equal<Actual_Mul_Short_Neg, "1111111111111101">
