import type {Equal} from '../../src/util.js'
import type {BitShiftLeftOne, BitShiftLeft} from '../../src/bit/index.js'

const bitshiftlg0: true = {} as Equal<BitShiftLeftOne<`1111`>, `1110`>
const bitshiftlg1: true = {} as Equal<BitShiftLeftOne<`0000`>, `0000`>
const bitshiftlg2: true = {} as Equal<BitShiftLeftOne<`1010`>, `0100`>

const bitshiftl0: true = {} as Equal<BitShiftLeft<`1111`, [[null]]>, `1100`>
const bitshiftl1: true = {} as Equal<BitShiftLeft<`0000`, [null]>, `0000`>
const bitshiftl2: true = {} as Equal<BitShiftLeft<`1010`, [null]>, `0100`>
const bitshiftl3: true = {} as Equal<BitShiftLeft<`1111`, [[[null]]]>, `1000`>
const bitshiftl4: true = {} as Equal<BitShiftLeft<`1111`, [[[[null]]]]>, `0000`>
const bitshiftl5: true = {} as Equal<BitShiftLeft<`1111`, [[[[[null]]]]]>, `0000`>
