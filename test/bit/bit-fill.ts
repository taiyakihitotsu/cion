import type {Equal} from '../../src/util.js'
import type {BitFill} from '../../src/bit/index.js'
import type * as Peano from "../../src/peano.js";

const bitfill0: true = {} as Equal<BitFill<"1111", Peano.T8>, "00001111">
const bitfill1: true = {} as Equal<BitFill<"0000", Peano.T8>, "00000000">
const bitfill2: true = {} as Equal<BitFill<"111", Peano.T8>, "00000111">
const bitfill3: true = {} as Equal<BitFill<"11", Peano.T8>, "00000011">
const bitfill4: true = {} as Equal<BitFill<"1", Peano.T8>, "00000001">

type T2 = [[null]]
type T4 = [[[[null]]]]

type Actual_Fill_Exact = BitFill<"1010", T4>
const testbitfill_exact: true = {} as Equal<Actual_Fill_Exact, "1010">

type Actual_Fill_Small = BitFill<"1", T2>
const testbitfill_small: true = {} as Equal<Actual_Fill_Small, "01">

type Actual_Fill_Zero = BitFill<"111", Peano.T0>
const testbitfill_zero: true = {} as Equal<Actual_Fill_Zero, "">

type Actual_Fill_Empty = BitFill<"", T4>
const testbitfill_empty: true = {} as Equal<Actual_Fill_Empty, "0000">

type Actual_Fill_Overflow = BitFill<"11111", T4>
const testbitfill_overflow: true = {} as Equal<Actual_Fill_Overflow, "1111">

type Actual_Fill_Neg = BitFill<"111", T4>
const testbitfill_neg: true = {} as Equal<Actual_Fill_Neg, "0111">

type Actual_Fill_Large = BitFill<"101", Peano.T8>
const testbitfill_large: true = {} as Equal<Actual_Fill_Large, "00000101">
