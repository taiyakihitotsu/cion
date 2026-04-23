import type {Equal} from '../../src/util.js'
import type {BitAnd} from '../../src/bit/index.js'

const bitand1: true = {} as Equal<BitAnd<`1`, `1`>, `1`>
const bitand2: true = {} as Equal<BitAnd<`1`, `0`>, `0`>
const bitand3: true = {} as Equal<BitAnd<`0`, `1`>, `0`>
const bitand4: true = {} as Equal<BitAnd<`0`, `0`>, `0`>
const bitand5: true = {} as Equal<BitAnd<`010`, `000`>, `000`>
const bitand6: true = {} as Equal<BitAnd<`111`, `111`>, `111`>
const bitand7: true = {} as Equal<BitAnd<`110`, `110`>, `110`>
const bitand8: true = {} as Equal<BitAnd<`000`, `000`>, `000`>
