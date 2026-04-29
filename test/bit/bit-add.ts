import type {Equal} from '../../src/util.js'
import type {BitAdd, _BitAdd, } from '../../src/bit/index.js'
import {CurPad} from '../../src/bit/index.js'

const _bitadd0: true = {} as Equal<_BitAdd<"00111", "00101">, "01100">
const _bitadd1: true = {} as Equal<_BitAdd<"00110", "00001">, "00111">
const _bitadd2: true = {} as Equal<_BitAdd<"00000", "00000">, "00000">
const _bitadd3: true = {} as Equal<_BitAdd<"11111", "11111">, "11110">

// 7,5,12
// 6,1,7
// 0,0,0
// 31,31,62
const bitadd0: true = {} as Equal<BitAdd<"00111", "00101">, `${typeof CurPad}00001100`>
const bitadd1: true = {} as Equal<BitAdd<"00110", "00001">, `${typeof CurPad}00000111`>
const bitadd2: true = {} as Equal<BitAdd<"00000", "00000">, `${typeof CurPad}00000000`>
const bitadd3: true = {} as Equal<BitAdd<"11111", "11111">, `${typeof CurPad}00111110`> // shift.
const bitadd4: true = {} as Equal<BitAdd<'1111111111111111', '0000000000000011'>, '0000000000000010'>
const bitadd5: true = {} as Equal<BitAdd<'0000000000000011','1111111111111111'>, '0000000000000010'>
const bitadd6: true = {} as Equal<BitAdd<'1111111111111111', '1111111111111111'>, '1111111111111110'>

// 0111 + 0001 = 1000 (All bits carry over)
const _bitadd_carry_chain: true = {} as Equal<_BitAdd<"0111", "0001">, "1000">

// 1111 + 0001 = 0000 (Full carry chain to zero)
const _bitadd_full_carry: true = {} as Equal<_BitAdd<"1111", "0001">, "0000">

// Max Positive + 1 = Min Negative (Standard 2's complement wrap)
// 0111...111 + 000...001 = 1000...000
const bitadd_wrap_to_min: true = {} as Equal<
  BitAdd<'0111111111111111', '0000000000000001'>, 
  '1000000000000000'
>

// Min Negative + Min Negative = 0 (Wrap around)
// 1000...000 + 1000...000 = 0000...000
const bitadd_min_min: true = {} as Equal<
  BitAdd<'1000000000000000', '1000000000000000'>, 
  '0000000000000000'
>

// Near Max Negative
// -2 + 1 = -1
const bitadd_neg_inc: true = {} as Equal<
  BitAdd<'1111111111111110', '0000000000000001'>, 
  '1111111111111111'
>

// Testing if BitAdd correctly handles operands of different lengths with CurPad
// "1" (extended to 16bit) + "1" (extended to 16bit) = 2
const bitadd_different_lengths: true = {} as Equal<
  BitAdd<"1", "0001">, 
  `${typeof CurPad}00000010`
>

// All zeros with different length
const bitadd_zero_ext: true = {} as Equal<
  BitAdd<"0", "00000">, 
  `${typeof CurPad}00000000`
>
