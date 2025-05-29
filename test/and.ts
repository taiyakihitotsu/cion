import type Cion from '../src/index'
import type { LispAnd } from '../src/index'

const lispandtest1: LispAnd<[[`prim`, true], [`prim`, true]]> = [`prim`, true];
const lispandtest2: LispAnd<[[`prim`, true], [`prim`, false]]> = [`prim`, false];
const lispandtest2_1: LispAnd<[[`prim`, false], [`prim`, true]]> = [`prim`, false];
const lispandtest3: LispAnd<[[`prim`, true], [`prim`, false], [`prim`, true]]> =
  [`prim`, false];
const lispandtest3_1: LispAnd<[[`prim`, false], [`prim`, false], [`prim`, true]]> =
  [`prim`, false];
const lispandtest4: LispAnd<[[`prim`, true], [`prim`, true], [`prim`, true]]> =
  [`prim`, true];
const lispandtest5: LispAnd<[[`prim`, true], [`prim`, "nil"]]> = [`prim`, false];
const lispandtest6: LispAnd<[[`prim`, "nil"], [`prim`, "''"]]> = [`prim`, false];
const lispandtest7: LispAnd<[[`prim`, "nil"], [`prim`, "nil"]]> = [`prim`, false];
const lispandtest8: LispAnd<[[`prim`, "nil"], [`prim`, false]]> = [`prim`, false];
const lispandtest9: LispAnd<[[`prim`, false], [`prim`, "nil"]]> = [`prim`, false];

const maintest7_and: Cion.RawLisp<"(and true true)"> = ['prim', true]
const maintest8_and: Cion.RawLisp<"(and true false)"> = ['prim', false]
const maintest9_and: Cion.RawLisp<"(and false false)"> = ['prim', false]
const maintest10_and: Cion.RawLisp<"(and false true)"> = ['prim', false]
const maintest7_1_and: Cion.RawLisp<"(and true true true)"> = ['prim', true]
const maintest8_1_and: Cion.RawLisp<"(and false true false)"> = ['prim', false]
const maintest9_1_and: Cion.RawLisp<"(and false false false)"> = ['prim', false]
const maintest10_1_and: Cion.RawLisp<"(and false true true)"> = ['prim', false]
