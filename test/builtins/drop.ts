import type Cion from '../../src/index.js'
import type { Drop } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const drop_test_0 : true = {} as Equal<[3, 4, 5, 6], 
Drop<"11", [0, 1, 2, 3, 4, 5, 6]>>

const drop_test_1 : true = {} as Equal<['vec'], 
Cion.RawLisp<'(drop 9 [0 1 2 3 4 5])'>>

const drop_test_2 : true = {} as Equal<['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']], 
Cion.RawLisp<'(drop 2 [0 1 2 3 4 5])'>>

const drop_test_3 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']], 
Cion.RawLisp<'(drop 0 [0 1 2 3 4 5])'>>

const drop_test_4 : true = {} as Equal<'[0 1 2 3 4 5 6]', 
Cion.Lisp<`(drop 0 [0 1 2 3 4 5 6])`>>

const drop_test_5 : true = {} as Equal<'[2 3 4 5 6]', 
Cion.Lisp<`(drop 2 [0 1 2 3 4 5 6])`>>

const drop_test_6 : true = {} as Equal<'[]', 
Cion.Lisp<`(drop 9 [0 1 2 3 4 5 6])`>>

const drop_test_7 : true = {} as Equal<'[0 1 2 3 4 5 6]', 
Cion.Lisp<`(drop -3 [0 1 2 3 4 5 6])`>>
