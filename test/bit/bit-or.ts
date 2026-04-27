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

type Actual_Or_Mixed = BitOr<"1010", "0110">
const testbitor_mixed: true = {} as Equal<Actual_Or_Mixed, "1110">

type Actual_Or_DiffLen = BitOr<"101", "0">
// @ts-expect-error: 
const testbitor_difflen_t: true = {} as Equal<Actual_Or_DiffLen, "101">

type Actual_Or_AllOnes = BitOr<"1111", "0000">
const testbitor_allones: true = {} as Equal<Actual_Or_AllOnes, "1111">

type Actual_Or_LeadingZeros = BitOr<"0011", "0101">
const testbitor_leading: true = {} as Equal<Actual_Or_LeadingZeros, "0111">

type Actual_Or_Long = BitOr<"0000111100001111", "1111000011110000">
const testbitor_long: true = {} as Equal<Actual_Or_Long, "1111111111111111">

type Actual_Or_Empty = BitOr<"", "101">
const testbitor_empty: true = {} as Equal<Actual_Or_Empty, "">

type Actual_Or_Both_Empty = BitOr<"", "">
const testbitor_both_empty: true = {} as Equal<Actual_Or_Both_Empty, "">
