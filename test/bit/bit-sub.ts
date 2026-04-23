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

