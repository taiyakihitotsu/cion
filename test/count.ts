import type Cion from '../src/index'
import type { Count, LispCount } from '../src/index'

const testcount: Count<[0,1,2]> = "0000000000000011"

const testlcount: LispCount<[['vec', 0, 1, 2]]> = ['prim', "0000000000000011"]

const test_count0: Cion.RawLisp<`(count [0 1 2])`> = ['prim', '0000000000000011']

const test_raw_count0: Cion.Lisp<`(count [0 1 2])`> = '3'
