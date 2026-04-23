import type {Equal} from '../../src/util.js'
import type {_BitShiftRight} from '../../src/bit/index.js'

const bitsr0: true = {} as Equal<_BitShiftRight<"111111", [[[[[[null]]]]]], [[null]]>, "1111">
const bitsr1: true = {} as Equal<_BitShiftRight<"111111", [[[[[[null]]]]]], [[[null]]]>, "111">
