import type {Equal} from '../../src/util.js'
import type {BitGTE} from '../../src/bit/index.js'

const bitsub0gte: true = {} as Equal<BitGTE<"00111", "00101">, true>
const bitsub1gte: true = {} as Equal<BitGTE<"00110", "00001">, true>
const bitsub2gte: true = {} as Equal<BitGTE<"00000", "00000">, true>
const bitsub3gte: true = {} as Equal<BitGTE<"11111", "11111">, true>
const bitsub4gte: true = {} as Equal<BitGTE<"00111", "01000">, false>
const bitsub5gte: true = {} as Equal<BitGTE<"00000", "11111">, false>
