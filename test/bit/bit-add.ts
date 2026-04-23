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
