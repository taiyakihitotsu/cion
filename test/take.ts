import type Cion from '../src/index'
import type { Take } from '../src/index'

const testtake0: Take<"11", [0,1,2,3,4,5,6]> = [0,1,2]

const maintest0_take_0: Cion.RawLisp<'(take 0 [0 1 2 3 4 5])'> = ['vec']
const maintest0_take_1: Cion.RawLisp<'(take 2 [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001']]
const maintest0_take_2: Cion.RawLisp<'(take 9 [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']]
