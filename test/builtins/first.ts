import type Cion from '../../src/index.js'
import type { First } from '../../src/index.js'
import { VNil }  from '../../src/sexprtypes.js'
import type { Equal } from '../../src/util.js'

type testvec = [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1]];

const first_test_0 : true = {} as Equal<[`prim`, true]
  ,First<testvec>>

const first_test_1 : true = {} as Equal<['prim', '0000000000000000']
  ,Cion.RawLisp<'(first [0 1 2])'>>

const first_test_2 : true = {} as Equal<typeof VNil
  ,Cion.RawLisp<'(first [])'>>

const first_test_3 : true = {} as Equal<'0'
  ,Cion.Lisp<'(first [0 1 2])'>>

const first_test_4 : true = {} as Equal<'nil'
  ,Cion.Lisp<'(first [])'>>

// @ts-expect-error:
const maintest1_first_2: Cion.Lisp<'(first {:a 1})'> = 'nil'

