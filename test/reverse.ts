import type Cion from '../src/index'
import type { Reverse } from '../src/index'

const reversetest0: Reverse<[0,1,2,3,4]> = [4,3,2,1,0]
const reversetest1: Reverse<[]> = []
const reversetest2: Reverse<[1]> = [1]
const reversetest3: Reverse<[[1,2]]> = [[1,2]]

const maintest0_reverse_0: Cion.RawLisp<'(reverse [0 1 2 3])'> = ['vec', ['prim', '0000000000000011'], ['prim', '0000000000000010'], ['prim', '0000000000000001'],  ['prim', '0000000000000000']]
const maintest0_reverse_1: Cion.RawLisp<'(reverse [0])'> = ['vec',  ['prim', '0000000000000000']]
