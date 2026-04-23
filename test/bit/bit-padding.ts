import type {Equal} from '../../src/util.js'
import type {BitPadding} from '../../src/bit/index.js'

const bitpadding0: true = {} as Equal<BitPadding<"10101", [null]>, "010101">
const bitpadding1: true = {} as Equal<BitPadding<"10101", [[null]]>, "0010101">
const bitpadding2: true = {} as Equal<BitPadding<"10101", [[null]], "1">, "1110101">
