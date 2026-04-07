import type Cion from '../../src/index.js'
import type { LispOr } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const or_internal_test_0 : true = {} as Equal<['prim', true], LispOr<[[`prim`, true], [`prim`, true]]>>
const or_internal_test_1 : true = {} as Equal<['prim', true], LispOr<[[`prim`, true], [`prim`, false]]>>
const or_internal_test_2 : true = {} as Equal<['prim', true], LispOr<[[`prim`, false], [`prim`, true]]>>
const or_internal_test_3 : true = {} as Equal<['prim', true], LispOr<[[`prim`, true], [`prim`, false], [`prim`, true]]>>
const or_internal_test_4 : true = {} as Equal<['prim', true], LispOr<[[`prim`, false], [`prim`, false], [`prim`, true]]>>

// Truthiness with strings and nil
const or_internal_test_5 : true = {} as Equal<['prim', true],  LispOr<[[`prim`, true], [`prim`, "nil"]]> >
const or_internal_test_6 : true = {} as Equal<['prim', true],  LispOr<[[`prim`, "nil"], [`prim`, "''"]]> > // Empty string is truthy
const or_internal_test_7 : true = {} as Equal<['prim', false], LispOr<[[`prim`, "nil"], [`prim`, "nil"]]> >
const or_internal_test_8 : true = {} as Equal<['prim', false], LispOr<[[`prim`, "nil"], [`prim`, false]]> >

// RawLisp surface tests
const or_raw_test_0 : true = {} as Equal<['prim', true],  Cion.RawLisp<"(or true true)">>
const or_raw_test_1 : true = {} as Equal<['prim', true],  Cion.RawLisp<"(or true false)">>
const or_raw_test_2 : true = {} as Equal<['prim', false], Cion.RawLisp<"(or false false)">>
const or_raw_test_3 : true = {} as Equal<['prim', true],  Cion.RawLisp<"(or false true)">>
const or_raw_test_4 : true = {} as Equal<['prim', false], Cion.RawLisp<"(or false false false)">>
