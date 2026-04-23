import type {Equal} from '../../src/util.js'
import type {BitFill} from '../../src/bit/index.js'
import type * as Peano from "../../src/peano.js";

const bitfill0: true = {} as Equal<BitFill<"1111", Peano.T8>, "00001111">
const bitfill1: true = {} as Equal<BitFill<"0000", Peano.T8>, "00000000">
const bitfill2: true = {} as Equal<BitFill<"111", Peano.T8>, "00000111">
const bitfill3: true = {} as Equal<BitFill<"11", Peano.T8>, "00000011">
const bitfill4: true = {} as Equal<BitFill<"1", Peano.T8>, "00000001">
