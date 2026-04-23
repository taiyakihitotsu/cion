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

