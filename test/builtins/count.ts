import type Cion from '../../src/index.js'
import type { Count, LispCount } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const count_test_0 : true = {} as Equal<"0000000000000011", Count<[0, 1, 2]>>
const count_test_1 : true = {} as Equal<['prim', "0000000000000011"], LispCount<[['vec', ['prim', `'1'`], ['prim', `'2'`], ['prim', `'3'`]]]>>
const count_test_2 : true = {} as Equal<['prim', '0000000000000011'], Cion.RawLisp<`(count [0 1 2])`>>
const count_test_3 : true = {} as Equal<'3', Cion.Lisp<`(count [0 1 2])`>>
const count_test_4 : true = {} as Equal<'0', Cion.Lisp<`(count [])`>>

// @ts-expect-error
const test_raw_count1: Cion.Lisp<`(count {})`> = ''
// @ts-expect-error
const test_raw_count2: Cion.Lisp<`(count nil)`> = ''
