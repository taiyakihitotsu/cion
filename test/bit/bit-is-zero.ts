import type {Equal} from '../../src/util.js'
import type {BitIsZero} from '../../src/bit/index.js'

const bitiszero0: true = {} as Equal<BitIsZero<"111">, false>
const bitiszero1: true = {} as Equal<BitIsZero<"000">, true>

type Actual_IsZero_Single0 = BitIsZero<"0">
const testbitiszero_single0: true = {} as Equal<Actual_IsZero_Single0, true>

type Actual_IsZero_Single1 = BitIsZero<"1">
const testbitiszero_single1: true = {} as Equal<Actual_IsZero_Single1, false>

type Actual_IsZero_Empty = BitIsZero<"">
const testbitiszero_empty: true = {} as Equal<Actual_IsZero_Empty, false>

type Actual_IsZero_Mixed = BitIsZero<"0000000000000001">
const testbitiszero_mixed: true = {} as Equal<Actual_IsZero_Mixed, false>

type Actual_IsZero_Long = BitIsZero<"0000000000000000">
const testbitiszero_long: true = {} as Equal<Actual_IsZero_Long, true>

type Actual_IsZero_Trailing = BitIsZero<"1000">
const testbitiszero_trailing: true = {} as Equal<Actual_IsZero_Trailing, false>
