import type Cion from '../../src/index.ts'
import type { LispRelation } from '../../src/index.ts'
import type {Equal} from '../../src/util'

const gte_test_0 : true = {} as Equal<[`prim`, true],
LispRelation<'>=', [[`prim`, '00000011'], [`prim`, '0000001']]>>

const gte_test_1 : true = {} as Equal<[`prim`, true],
LispRelation<'>=', [[`prim`, '00001111'], [`prim`, '00000011'], [`prim`, '0000001']]>>

const gte_test_2 : true = {} as Equal<[`prim`, true],
LispRelation<'<=', [[`prim`, '00001111'], [`prim`, '00001111'], [`prim`, '00001111']]>>

const gte_test_3 : true = {} as Equal<[`prim`, true],
Cion.RawLisp<"(>= 15 14)">>

const gte_test_4 : true = {} as Equal<[`prim`, true],
Cion.RawLisp<"(>= 15 14 13)">>

const gte_test_5 : true = {} as Equal<[`prim`, false],
Cion.RawLisp<"(>= 13 14 15)">>

const gte_test_6 : true = {} as Equal<[`prim`, false],
Cion.RawLisp<"(>= 14 15)">>

const gte_test_7 : true = {} as Equal<[`prim`, false],
Cion.RawLisp<"(>= 13 14 15)">>

const gte_test_8 : true = {} as Equal<[`prim`, true],
Cion.RawLisp<"(>= 15 15 15)">>

const gte_test_9 : true = {} as Equal<[`prim`, true],
Cion.RawLisp<"(>= 15 15 14)">>

const gte_test_10 : true = {} as Equal<[`prim`, true],
Cion.RawLisp<"(>= 15 14 14)">>

const gte_test_11 : true = {} as Equal<[`prim`, false],
Cion.RawLisp<"(>= 15 14 15)">>
