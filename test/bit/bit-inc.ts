import type {Equal} from '../../src/util.js'
import type {BitInc} from '../../src/bit/index.js'

const testbitinc0: true = {} as Equal<BitInc<"0000000000000000">, "0000000000000001">
const testbitinc1: true = {} as Equal<BitInc<"1111111111111111">, "0000000000000000">
const testbitinc2: true = {} as Equal<BitInc<"0000000000000011">, "0000000000000100">
