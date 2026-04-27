import type {Equal} from '../../src/util.js'
import type {BitXor} from '../../src/bit/index.js'

const bitxor1: true = {} as Equal<BitXor<`1`, `1`>, `0`>
const bitxor2: true = {} as Equal<BitXor<`1`, `0`>, `1`>
const bitxor3: true = {} as Equal<BitXor<`0`, `1`>, `1`>
const bitxor4: true = {} as Equal<BitXor<`0`, `0`>, `0`>
const bitxor5: true = {} as Equal<BitXor<`010`, `000`>, `010`>
const bitxor6: true = {} as Equal<BitXor<`111`, `111`>, `000`>
const bitxor7: true = {} as Equal<BitXor<`110`, `110`>, `000`>
const bitxor8: true = {} as Equal<BitXor<`000`, `000`>, `000`>
const bitxor9: true = {} as Equal<BitXor<`101`, `001`>, `100`>
const bitxor10: true = {} as Equal<BitXor<`00111`, `00101`>, `00010`>

type Actual_Xor_Long = BitXor<"1010101010101010", "1111111100000000">
const testbitxor_long: true = {} as Equal<Actual_Xor_Long, "0101010110101010">

type Actual_Xor_DiffLen = BitXor<"111", "1">
// @ts-expect-error:
const testbitxor_difflen: true = {} as Equal<Actual_Xor_DiffLen, "110">

type Actual_Xor_Self = BitXor<"11001100", "11001100">
const testbitxor_self: true = {} as Equal<Actual_Xor_Self, "00000000">

type Actual_Xor_Neg1 = BitXor<"1111111111111111", "0000000000000000">
const testbitxor_neg1_zero: true = {} as Equal<Actual_Xor_Neg1, "1111111111111111">

type Actual_Xor_Empty = BitXor<"", "101">
const testbitxor_empty_empty: true = {} as Equal<Actual_Xor_Empty, "">
// @ts-expect-error:
const testbitxor_empty: true = {} as Equal<Actual_Xor_Empty, "101">

type Actual_Xor_LeadingZeros = BitXor<"0001", "1000">
const testbitxor_leading: true = {} as Equal<Actual_Xor_LeadingZeros, "1001">

type Actual_Xor_Toggle = BitXor<"10101010", "11111111">
const testbitxor_toggle: true = {} as Equal<Actual_Xor_Toggle, "01010101">
