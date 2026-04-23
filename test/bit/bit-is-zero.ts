import type {Equal} from '../../src/util.js'
import type {BitIsZero} from '../../src/bit/index.js'

const bitiszero0: true = {} as Equal<BitIsZero<"111">, false>
const bitiszero1: true = {} as Equal<BitIsZero<"000">, true>
