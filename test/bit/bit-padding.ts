import type {Equal} from '../../src/util.js'
import type {BitPadding} from '../../src/bit/index.js'

const bitpadding0: true = {} as Equal<BitPadding<"10101", [null]>, "010101">
const bitpadding1: true = {} as Equal<BitPadding<"10101", [[null]]>, "0010101">
const bitpadding2: true = {} as Equal<BitPadding<"10101", [[null]], "1">, "1110101">

type Actual_no_padding = BitPadding<"10101", null, "1">
const expected_no_padding: true = {} as Equal<"10101", Actual_no_padding>

type Actual_peano_error = 
  BitPadding<
    "10101"
  // @ts-expect-error:
  , []
  , "1">

