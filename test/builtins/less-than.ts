import type Cion from '../../src/index.js'
import type { LispRelation } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const lisp_less_test_0 : true =
  {} as Equal<[`prim`, false], LispRelation<'<', [[`prim`, '00000011'], [`prim`, '0000001']]>>

const lisp_less_test_1 : true =
  {} as Equal<[`prim`, false], LispRelation<'<', [[`prim`, '00001111'], [`prim`, '00000011'], [`prim`, '0000001']]>>

const lisp_less_test_2 : true =
  {} as Equal<[`prim`, false], LispRelation<'<', [[`prim`, '00001111'], [`prim`, '00001111'], [`prim`, '00001111']]>>

const raw_lisp_less_test_0 : true =
  {} as Equal<[`prim`, false], Cion.RawLisp<"(< 15 14)">>

const raw_lisp_less_test_1 : true =
  {} as Equal<[`prim`, false], Cion.RawLisp<"(< 15 14 13)">>

const raw_lisp_less_test_2 : true =
  {} as Equal<[`prim`, true], Cion.RawLisp<"(< 14 15)">>

const raw_lisp_less_test_3 : true =
  {} as Equal<[`prim`, true], Cion.RawLisp<"(< 13 14 15)">>

const raw_lisp_less_test_4 : true =
  {} as Equal<[`prim`, false], Cion.RawLisp<"(< 13 15 14)">>

const raw_lisp_less_test_5 : true =
  {} as Equal<[`prim`, false], Cion.RawLisp<"(< 15 14 13)">>
