import type {Equal} from '../../src/util.js'
import type {BitLen} from '../../src/bit/index.js'

const bitlen0: true = {} as Equal<BitLen<"00000000">, [[[[[[[[null]]]]]]]]>
const bitlen1: true = {} as Equal<BitLen<"">, null>
