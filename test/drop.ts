import type Cion from '../src/index'
import type { Drop } from '../src/index'

const testDrop0: Drop<"11", [0,1,2,3,4,5,6]> = [3,4,5,6]

const maintest0_drop_0: Cion.RawLisp<'(drop 9 [0 1 2 3 4 5])'> = ['vec']
const maintest0_drop_1: Cion.RawLisp<'(drop 2 [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']]
const maintest0_drop_2: Cion.RawLisp<'(drop 0 [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']]


const drop_test0 : Cion.Lisp<`(drop 0 [0 1 2 3 4 5 6])`> = '[0 1 2 3 4 5 6]'
const drop_test1 : Cion.Lisp<`(drop 2 [0 1 2 3 4 5 6])`> = '[2 3 4 5 6]'
const drop_test : Cion.Lisp<`(drop 9 [0 1 2 3 4 5 6])`> = '[]'
const drop_test2 : Cion.Lisp<`(drop -3 [0 1 2 3 4 5 6])`> = '[0 1 2 3 4 5 6]'

