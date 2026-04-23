import type {Equal} from '../../src/util.js'
import type {BitDec} from '../../src/bit/index.js'

const testbitdec0: true = {} as Equal<BitDec<"0000000000000001">, "0000000000000000">
const testbitdec1: true = {} as Equal<BitDec<"0000000000000000">, "1111111111111111">
const testbitdec2: true = {} as Equal<BitDec<"0000000000000100">, "0000000000000011">
