import type {Equal} from '../../src/util.js'
import type {BitEq} from '../../src/bit/index.js'

const biteq0: true = {} as Equal<BitEq<"0", "0">, true>
const biteq1: true = {} as Equal<BitEq<"0", "1">, false>
const biteq2: true = {} as Equal<BitEq<"1", "0">, false>
const biteq3: true = {} as Equal<BitEq<"1", "1">, true>
const biteq4: true = {} as Equal<BitEq<"00", "10">, false>
const biteq5: true = {} as Equal<BitEq<"11", "11">, true>
const biteq6: true = {} as Equal<BitEq<"01", "10">, false>
const biteq7: true = {} as Equal<BitEq<"", "1">, false>
const biteq8: true = {} as Equal<BitEq<"0", "">, false>
const biteq9: true = {} as Equal<BitEq<"", "">, true>

type Actual_Eq_Long = BitEq<"0000101010101010", "0000101010101010">
const testbiteq_long: true = {} as Equal<Actual_Eq_Long, true>

type Actual_Eq_DiffLen_SameValue = BitEq<"1", "0001">
const testbiteq_difflen_same: true = {} as Equal<Actual_Eq_DiffLen_SameValue, false>

type Actual_Eq_Neg1 = BitEq<"1111111111111111", "1111111111111111">
const testbiteq_neg1: true = {} as Equal<Actual_Eq_Neg1, true>

type Actual_Eq_Mixed_Bits = BitEq<"10101", "10111">
const testbiteq_mixed: true = {} as Equal<Actual_Eq_Mixed_Bits, false>

type Actual_Eq_LeadingZero = BitEq<"01", "1">
const testbiteq_leading_zero: true = {} as Equal<Actual_Eq_LeadingZero, false>

type Actual_Eq_AllZero = BitEq<"0000", "0000">
const testbiteq_allzero: true = {} as Equal<Actual_Eq_AllZero, true>
