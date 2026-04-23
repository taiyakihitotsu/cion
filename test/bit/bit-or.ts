import type {Equal} from '../../src/util.js'
import type {BitOr} from '../../src/bit/index.js'

const bitor1: true = {} as Equal<BitOr<`1`, `1`>, `1`>
const bitor2: true = {} as Equal<BitOr<`1`, `0`>, `1`>
const bitor3: true = {} as Equal<BitOr<`0`, `1`>, `1`>
const bitor4: true = {} as Equal<BitOr<`0`, `0`>, `0`>
const bitor5: true = {} as Equal<BitOr<`010`, `000`>, `010`>
const bitor6: true = {} as Equal<BitOr<`111`, `111`>, `111`>
const bitor7: true = {} as Equal<BitOr<`110`, `110`>, `110`>
const bitor8: true = {} as Equal<BitOr<`000`, `000`>, `000`>
