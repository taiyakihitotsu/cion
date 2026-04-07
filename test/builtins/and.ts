import type Cion from '../../src/index.js'
import type { LispAnd } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const and_test_0: true = {} as Equal<[`prim`, true], LispAnd<[[`prim`, true], [`prim`, true]]>>
const and_test_1: true = {} as Equal<[`prim`, false], LispAnd<[[`prim`, true], [`prim`, false]]>>
const and_test_2: true = {} as Equal<[`prim`, false], LispAnd<[[`prim`, false], [`prim`, true]]>>
const and_test_3: true = {} as Equal<[`prim`, false], LispAnd<[[`prim`, true], [`prim`, false], [`prim`, true]]>>
const and_test_4: true = {} as Equal<[`prim`, false], LispAnd<[[`prim`, false], [`prim`, false], [`prim`, true]]>>
const and_test_5: true = {} as Equal<[`prim`, true], LispAnd<[[`prim`, true], [`prim`, true], [`prim`, true]]>>

const and_test_6: true = {} as Equal<[`prim`, false], LispAnd<[[`prim`, true], [`prim`, "nil"]]>>
const and_test_7: true = {} as Equal<[`prim`, false], LispAnd<[[`prim`, "nil"], [`prim`, "''"]]>>
const and_test_8: true = {} as Equal<[`prim`, false], LispAnd<[[`prim`, "nil"], [`prim`, "nil"]]>>
const and_test_9: true = {} as Equal<[`prim`, false], LispAnd<[[`prim`, "nil"], [`prim`, false]]>>
const and_test_10: true = {} as Equal<[`prim`, false], LispAnd<[[`prim`, false], [`prim`, "nil"]]>>

const and_test_11: true = {} as Equal<['prim', true], Cion.RawLisp<"(and true true)">>
const and_test_12: true = {} as Equal<['prim', false], Cion.RawLisp<"(and true false)">>
const and_test_13: true = {} as Equal<['prim', false], Cion.RawLisp<"(and false false)">>
const and_test_14: true = {} as Equal<['prim', false], Cion.RawLisp<"(and false true)">>

const and_test_15: true = {} as Equal<['prim', true], Cion.RawLisp<"(and true true true)">>
const and_test_16: true = {} as Equal<['prim', false], Cion.RawLisp<"(and false true false)">>
const and_test_17: true = {} as Equal<['prim', false], Cion.RawLisp<"(and false false false)">>
const and_test_18: true = {} as Equal<['prim', false], Cion.RawLisp<"(and false true true)">>

const and_test_19: true = {} as Equal<'true', Cion.Lisp<"(and true (if 1 1 1))">>
const and_test_20: true = {} as Equal<'true', Cion.Lisp<"((fn [n] (and n (if 1 1 1))) true)">>
const and_test_21: true = {} as Equal<'true', Cion.Lisp<"((fn [n m] (and n (if m 1 1))) true true)">>
const and_test_22: true = {} as Equal<'true', Cion.Lisp<"((fn [n m] (and n m)) true (if 9 9 9))">>
const and_test_23: true = {} as Equal<'true', Cion.Lisp<"((fn [n m] (and n (if m 1 1))) true (if 9 9 9))">>

const and_test_24: true = {} as Equal<'true', Cion.Lisp<`((fn [x y] (and (number? x) (number? y))) 9 (if 9 9 9))`>>
const and_test_25: true = {} as Equal<'true', Cion.Lisp<`((fn [x y] (and (number? x) (number? y))) 9 (inc 9))`>>
const and_test_26: true = {} as Equal<'true', Cion.Lisp<`((fn [x y] (and (number? x) (number? y))) 9 (+ 9 9))`>>
