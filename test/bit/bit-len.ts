import type {Equal} from '../../src/util.js'
import type {BitLen} from '../../src/bit/index.js'

const bitlen0: true = {} as Equal<BitLen<"00000000">, [[[[[[[[null]]]]]]]]>
const bitlen1: true = {} as Equal<BitLen<"">, null>

type Actual_Len_1 = BitLen<"1">
const testbitlen_1: true = {} as Equal<Actual_Len_1, [null]>

type Actual_Len_2 = BitLen<"10">
const testbitlen_2: true = {} as Equal<Actual_Len_2, [[null]]>

type Actual_Len_3 = BitLen<"101">
const testbitlen_3: true = {} as Equal<Actual_Len_3, [[[null]]]>

type Actual_Len_4 = BitLen<"0000">
const testbitlen_4: true = {} as Equal<Actual_Len_4, [[[[null]]]]>

type Actual_Len_16 = BitLen<"0000000000000000">
const testbitlen_16: true = {} as Equal<
  Actual_Len_16, 
  [[[[[[[[[[[[[[[[null]]]]]]]]]]]]]]]]
>

type Actual_Len_LeadingZeros = BitLen<"001">
const testbitlen_leading: true = {} as Equal<Actual_Len_LeadingZeros, [[[null]]]>
