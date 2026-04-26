import type {Equal} from '../../src/util.js'
import type { BitLenGthan} from '../../src/bit/index.js'

const bitgthan0: true = {} as Equal<BitLenGthan<"000", "000">, false>
const bitgthan00: true = {} as Equal<BitLenGthan<"0000", "0000">, false>
const bitgthan1: true = {} as Equal<BitLenGthan<"000", "0001">, false>
const bitgthan2: true = {} as Equal<BitLenGthan<"0001", "000">, true>
const bitgthan3: true = {} as Equal<BitLenGthan<"", "000">, false>
const bitgthan4: true = {} as Equal<BitLenGthan<"", "">, false>
const bitgthan5: true = {} as Equal<BitLenGthan<"1", "">, false>

type Actual_Gthan_Long = BitLenGthan<"00000", "000">
const testbitgthan_long: true = {} as Equal<Actual_Gthan_Long, true>

type Actual_Gthan_Short = BitLenGthan<"0", "000">
const testbitgthan_short: true = {} as Equal<Actual_Gthan_Short, false>

type Actual_Gthan_Large_Diff = BitLenGthan<"1111111111111111", "1">
const testbitgthan_large_diff: true = {} as Equal<Actual_Gthan_Large_Diff, true>

type Actual_Gthan_Single_Empty = BitLenGthan<"0", "">
const testbitgthan_single_empty: true = {} as Equal<Actual_Gthan_Single_Empty, false>

type Actual_Gthan_Empty_Single = BitLenGthan<"", "0">
const testbitgthan_empty_single: true = {} as Equal<Actual_Gthan_Empty_Single, false>

type Actual_Gthan_Equal_Len_Different_Content = BitLenGthan<"111", "000">
const testbitgthan_equal_len: true = {} as Equal<Actual_Gthan_Equal_Len_Different_Content, false>
