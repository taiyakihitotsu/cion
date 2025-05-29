import type Cion from '../src/index'
import type { LispOr } from '../src/index'

const lisportest1: LispOr<[[`prim`, true], [`prim`, true]]> = [`prim`, true];
const lisportest2: LispOr<[[`prim`, true], [`prim`, false]]> = [`prim`, true];
const lisportest2_1: LispOr<[[`prim`, false], [`prim`, true]]> = [`prim`, true];
const lisportest3: LispOr<[[`prim`, true], [`prim`, false], [`prim`, true]]> =
  [`prim`, true];
const lisportest3_1: LispOr<[[`prim`, false], [`prim`, false], [`prim`, true]]> =
  [`prim`, true];
const lisportest4: LispOr<[[`prim`, true], [`prim`, true], [`prim`, true]]> =
  [`prim`, true];
const lisportest5: LispOr<[[`prim`, true], [`prim`, "nil"]]> = [`prim`, true];
const lisportest6: LispOr<[[`prim`, "nil"], [`prim`, "''"]]> = [`prim`, true];
const lisportest7: LispOr<[[`prim`, "nil"], [`prim`, "nil"]]> = [`prim`, false];
const lisportest8: LispOr<[[`prim`, "nil"], [`prim`, false]]> = [`prim`, false];
const lisportest9: LispOr<[[`prim`, false], [`prim`, "nil"]]> = [`prim`, false];

const maintest7_or: Cion.RawLisp<"(or true true)"> = ['prim', true]
const maintest8_or: Cion.RawLisp<"(or true false)"> = ['prim', true]
const maintest9_or: Cion.RawLisp<"(or false false)"> = ['prim', false]
const maintest10_or: Cion.RawLisp<"(or false true)"> = ['prim', true]
const maintest7_1_or: Cion.RawLisp<"(or true true true)"> = ['prim', true]
const maintest8_1_or: Cion.RawLisp<"(or false true false)"> = ['prim', true]
const maintest9_1_or: Cion.RawLisp<"(or false false false)"> = ['prim', false]
const maintest10_1_or: Cion.RawLisp<"(or false true true)"> = ['prim', true]
