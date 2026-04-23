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
