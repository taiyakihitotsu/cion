import type {Equal} from '../../src/util.js'
import type {BitInc} from '../../src/bit/index.js'

const testbitinc0: true = {} as Equal<BitInc<"0000000000000000">, "0000000000000001">
const testbitinc1: true = {} as Equal<BitInc<"1111111111111111">, "0000000000000000">
const testbitinc2: true = {} as Equal<BitInc<"0000000000000011">, "0000000000000100">

type BitMax = "0111111111111111"
type BitMin = "1000000000000000"

type Actual_Inc_Max = BitInc<BitMax>
const testbitinc_max: true = {} as Equal<Actual_Inc_Max, BitMin>

type Actual_Inc_Carry_Chain = BitInc<"01011">
const testbitinc_carry: true = {} as Equal<Actual_Inc_Carry_Chain, "0000000000001100">

type Actual_Inc_Neg = BitInc<"1111111111111110">
const testbitinc_neg: true = {} as Equal<Actual_Inc_Neg, "1111111111111111">

type Actual_Inc_Short = BitInc<"1">
const testbitinc_short: true = {} as Equal<Actual_Inc_Short, "0000000000000010">

type Actual_Inc_AllZeros_Short = BitInc<"000">
const testbitinc_zeros_short: true = {} as Equal<Actual_Inc_AllZeros_Short, "0000000000000001">

type Actual_Inc_Overflow_Short = BitInc<"1111111111111111">
const testbitinc_overflow_short: true = {} as Equal<Actual_Inc_Overflow_Short, "0000000000000000">
