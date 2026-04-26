import type {Equal} from '../../src/util.js'
import type {BitDiv} from '../../src/bit/index.js'
import type { TNil } from '../../src/sexprtypes.js'
import {CurPad} from '../../src/bit/index.js'

const testbitdiv0: true = {} as Equal<BitDiv<"00001001", "00000001">, `${typeof CurPad}00001001`>
const testbitdiv1: true = {} as Equal<BitDiv<"00001001", "00000011">, `${typeof CurPad}00000011`>
const testbitdiv2: true = {} as Equal<BitDiv<"00001001", "00000010">, `${typeof CurPad}00000100`>
const testbitdiv3: true = {} as Equal<BitDiv<"00001001", "00000000">, TNil>
const testbitdiv4: true = {} as Equal<BitDiv<"00000010", "00001010">, `${typeof CurPad}00000000`>
const testbitdiv5: true = {} as Equal<BitDiv<'0000000000000110', '1111111111111110'>, '1111111111111101'>
const testbitdiv6: true = {} as Equal<BitDiv<'1111111111111010', '00000000000000010'>, '1111111111111101'>
const testbitdiv7: true = {} as Equal<BitDiv<'1111111111111010', '1111111111111110'>, '0000000000000011'>

type Actual_Div_Same = BitDiv<"00001100", "00001100">
const testbitdiv_same: true = {} as Equal<Actual_Div_Same, `${typeof CurPad}00000001`>

type Actual_Div_One_Neg = BitDiv<"1111111111111111", "0000000000000001">
const testbitdiv_one_neg: true = {} as Equal<Actual_Div_One_Neg, "1111111111111111">

type Actual_Div_Neg_Self = BitDiv<"1111111111111111", "1111111111111111">
const testbitdiv_neg_self: true = {} as Equal<Actual_Div_Neg_Self, "0000000000000001">

type Actual_Div_Max_Min = BitDiv<"0111111111111111", "1000000000000000">
const testbitdiv_max_min: true = {} as Equal<Actual_Div_Max_Min, "0000000000000001">

type Actual_Div_Large_Dividend = BitDiv<"0100000000000000", "0000000000000010">
const testbitdiv_large: true = {} as Equal<Actual_Div_Large_Dividend, "0010000000000000">

type Actual_Div_Negative_Result = BitDiv<"0000000000001010", "1111111111111011">
const testbitdiv_neg_res: true = {} as Equal<Actual_Div_Negative_Result, "1111111111111110">

type Actual_Div_Zero_Dividend = BitDiv<"0000000000000000", "0000000000001111">
const testbitdiv_zero_dividend: true = {} as Equal<Actual_Div_Zero_Dividend, "0000000000000000">
