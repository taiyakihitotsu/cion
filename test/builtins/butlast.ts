import type Cion from '../../src/index'
import type { Butlast } from '../../src/index'
import { Equal } from '../../src/util'

type testbutlastvec = [`vec`, [`prim`, 0], [`prim`, 1], ['prim', 2], ['prim', 3]] 

const butlast_test_0 : true = {} as Equal<[`vec`, [`prim`, 0], [`prim`, 1], [`prim`, 2]], Butlast<testbutlastvec>>

const butlast_test_1 : true = {} as Equal<[`vec`, [`prim`, 0], [`prim`, 1]], Butlast<Butlast<testbutlastvec>>>

const butlast_test_2 : true = {} as Equal<[`vec`, [`prim`, 0]],  Butlast<Butlast<Butlast<testbutlastvec>>>>

const butlast_test_3 : true = {} as Equal<[`vec`], Butlast<Butlast<Butlast<Butlast<testbutlastvec>>>>>

const butlast_test_4 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']], Cion.RawLisp<'(butlast [0 1 2 3])'>>

const butlast_test_5 : true = {} as Equal<['vec'], Cion.RawLisp<'(butlast [0])'>>

const butlast_test_6 : true = {} as Equal<'[0 1]', Cion.Lisp<`(butlast [0 1 2])`>>
const butlast_test_7 : true = {} as Equal<'[]', Cion.Lisp<`(butlast [0])`>>
const butlast_test_8 : true = {} as Equal<'nil', Cion.Lisp<`(butlast [])`>>
