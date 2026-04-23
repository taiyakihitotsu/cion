import type { Equal } from '../../src/util.js'
import type { PeanoToDecimal } from '../../src/decimal/index.js'

const Expected_0_ok: true = {} as Equal<PeanoToDecimal<null>, 0>
const Expected_1_ok: true = {} as Equal<PeanoToDecimal<[null]>, 1>

