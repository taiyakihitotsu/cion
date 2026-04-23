import type {Equal} from '../../src/util.js'
import type {BitCut} from '../../src/bit/index.js'
import type * as Peano from "../../src/peano.js";

const bitcut0: true = {} as Equal<BitCut<"11111", Peano.T0>, "11111">
const bitcut1: true = {} as Equal<BitCut<"11111", [null]>, "1111">
// const bitcut2: true = {} as Equal<BitCut<"11111", [null]>, "111" // err
// const bitcut3: true = {} as Equal<BitCut<"", [null]>, null as never
