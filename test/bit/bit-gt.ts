import type {Equal} from '../../src/util.js'
import type {BitGT} from '../../src/bit/index.js'

const bitsub0gt: true = {} as Equal<BitGT<"00111", "00101">, true>
const bitsub1gt: true = {} as Equal<BitGT<"00110", "00001">, true>
const bitsub2gt: true = {} as Equal<BitGT<"00000", "00000">, false>
const bitsub3gt: true = {} as Equal<BitGT<"11111", "11111">, false>
const bitsub4gt: true = {} as Equal<BitGT<"00111", "01000">, false>
const bitsub5gt: true = {} as Equal<BitGT<"00000", "11111">, false>
