import type {Equal} from '../../src/util.js'
import type {BitLTE} from '../../src/bit/index.js'

const bitsub0lte: true = {} as Equal<BitLTE<"00111", "00101">, false>
const bitsub1lte: true = {} as Equal<BitLTE<"00110", "00001">, false>
const bitsub2lte: true = {} as Equal<BitLTE<"00000", "00000">, true>
const bitsub3lte: true = {} as Equal<BitLTE<"11111", "11111">, true>
const bitsub4lte: true = {} as Equal<BitLTE<"00111", "01000">, true>
const bitsub5lte: true = {} as Equal<BitLTE<"00000", "11111">, true>
