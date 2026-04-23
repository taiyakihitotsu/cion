import type {Equal} from '../../src/util.js'
import type {BitMod} from '../../src/bit/index.js'
import {CurPad} from '../../src/bit/index.js'
import type { TNil } from '../../src/sexprtypes.js'

const testbitmod0: true = {} as Equal<BitMod<"00001001", "00000001">,  `${typeof CurPad}00000000`>
const testbitmod1: true = {} as Equal<BitMod<"00001001", "00000011">,  `${typeof CurPad}00000000`>
const testbitmod2: true = {} as Equal<BitMod<"00001001", "00000010">,  `${typeof CurPad}00000001`>
const testbitmod3: true = {} as Equal<BitMod<"00001001", "00000000">, TNil>
const testbitmod4: true = {} as Equal<BitMod<"00000010", "00001010">,  `${typeof CurPad}00000010`>
const testbitmod5: true = {} as Equal<BitMod<"1111111111111110", "1111111111111010">,  `${typeof CurPad}00000010`>

