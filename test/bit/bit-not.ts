import type {Equal} from '../../src/util.js'
import type {BitNot} from '../../src/bit/index.js'

const bitnot0: true = {} as Equal<BitNot<"0">, "1">
const bitnot1: true = {} as Equal<BitNot<"1">, "0">
const bitnot2: true = {} as Equal<BitNot<"11000">, "00111">

type Actual_Not_AllOnes = BitNot<"1111111111111111">
const testbitnot_allones: true = {} as Equal<Actual_Not_AllOnes, "0000000000000000">

type Actual_Not_AllZeros = BitNot<"0000000000000000">
const testbitnot_allzeros: true = {} as Equal<Actual_Not_AllZeros, "1111111111111111">

type Actual_Not_Mixed = BitNot<"10101010">
const testbitnot_mixed: true = {} as Equal<Actual_Not_Mixed, "01010101">

type Actual_Not_Empty = BitNot<"">
const testbitnot_empty: true = {} as Equal<Actual_Not_Empty, never>

type Actual_Not_Boundary = BitNot<"0111111111111111">
const testbitnot_boundary: true = {} as Equal<Actual_Not_Boundary, "1000000000000000">

type Actual_Not_LeadingZeros = BitNot<"0001">
const testbitnot_leading: true = {} as Equal<Actual_Not_LeadingZeros, "1110">
