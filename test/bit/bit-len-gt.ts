import type {Equal} from '../../src/util.js'
import type { BitLenGthan} from '../../src/bit/index.js'

const bitgthan0: true = {} as Equal<BitLenGthan<"000", "000">, false>
const bitgthan00: true = {} as Equal<BitLenGthan<"0000", "0000">, false>
const bitgthan1: true = {} as Equal<BitLenGthan<"000", "0001">, false>
const bitgthan2: true = {} as Equal<BitLenGthan<"0001", "000">, true>
const bitgthan3: true = {} as Equal<BitLenGthan<"", "000">, false>
const bitgthan4: true = {} as Equal<BitLenGthan<"", "">, false>
const bitgthan5: true = {} as Equal<BitLenGthan<"1", "">, false>
