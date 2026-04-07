import type Cion from '../../src/index.js'
import { VNil } from '../../src/sexprtypes.js'
import type { Equal } from '../../src/util.js'

const last_test_0 : true =
  {} as Equal<['prim', '0000000000000010'], Cion.RawLisp<'(last [0 1 2])'>>

const last_test_1 : true =
  {} as Equal<typeof VNil, Cion.RawLisp<'(last [])'>>

const last_lisp_test_0 : true =
  {} as Equal<'2', Cion.Lisp<'(last [0 1 2])'>>

const last_lisp_test_1 : true =
  {} as Equal<'nil', Cion.Lisp<'(last [])'>>

// @ts-expect-error
expectType<'nil'>( {} as Cion.Lisp<'(last {:a 1})'> )
