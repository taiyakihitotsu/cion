import type {Equal} from '../../src/util.js'
import type {BitXor} from '../../src/bit/index.js'

const bitxor1: true = {} as Equal<BitXor<`1`, `1`>, `0`>
const bitxor2: true = {} as Equal<BitXor<`1`, `0`>, `1`>
const bitxor3: true = {} as Equal<BitXor<`0`, `1`>, `1`>
const bitxor4: true = {} as Equal<BitXor<`0`, `0`>, `0`>
const bitxor5: true = {} as Equal<BitXor<`010`, `000`>, `010`>
const bitxor6: true = {} as Equal<BitXor<`111`, `111`>, `000`>
const bitxor7: true = {} as Equal<BitXor<`110`, `110`>, `000`>
const bitxor8: true = {} as Equal<BitXor<`000`, `000`>, `000`>
const bitxor9: true = {} as Equal<BitXor<`101`, `001`>, `100`>
const bitxor10: true = {} as Equal<BitXor<`00111`, `00101`>, `00010`>

