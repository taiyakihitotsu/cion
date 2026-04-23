import type {Equal} from '../../src/util.js'
import type {BitLT} from '../../src/bit/index.js'

const bitsub0lt: true = {} as Equal<BitLT<"00111", "00101">, false>
const bitsub1lt: true = {} as Equal<BitLT<"00110", "00001">, false>
const bitsub2lt: true = {} as Equal<BitLT<"00000", "00000">, false>
const bitsub3lt: true = {} as Equal<BitLT<"11111", "11111">, false>
const bitsub4lt: true = {} as Equal<BitLT<"00111", "01000">, true>
const bitsub5lt: true = {} as Equal<BitLT<"00000", "11111">, true>
