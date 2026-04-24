import type {Equal} from '../../src/util.js'
import type { BitAbs, Neg1, BitOne } from '../../src/bit/index.js'
import {CurPad} from '../../src/bit/index.js'

const expected_7: true = {} as Equal<BitAbs<"00111">, "00111">
const expected_0: true = {} as Equal<BitAbs<"00000">, "00000">
const expected_1: true = {} as Equal<BitAbs<Neg1>, BitOne>
