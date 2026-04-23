import type {Equal} from '../../src/util.js'
import type {BitNot} from '../../src/bit/index.js'

// test
const bitnot0: true = {} as Equal<BitNot<"0">, "1">
const bitnot1: true = {} as Equal<BitNot<"1">, "0">
const bitnot2: true = {} as Equal<BitNot<"11000">, "00111">
