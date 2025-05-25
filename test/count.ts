import type Cion from '../src/index.ts'

const test_count0: Cion.RawLisp<`(count [0 1 2])`> = ['prim', '0000000000000011']

const test_raw_count0: Cion.Lisp<`(count [0 1 2])`> = '3'
