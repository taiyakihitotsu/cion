import type Cion from '../src/index'
import type { LispDiv } from '../src/index'

const testlispdiv0: LispDiv<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, ['0000000000000011', '0000000000000001']]
const testlispdiv1: LispDiv<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, ['0000000000001111', '0000000000000011']]
const testlispdiv2: LispDiv<[[`prim`, '00000011'], [`prim`, '0000000']]> = [`prim`, 'nil']

const maintest0_div: Cion.RawLisp<"(/ 4 1)"> = ['prim', ["0000000000000100", '0000000000000001']]
const maintest1_div: Cion.RawLisp<"(/ 1 0)"> = ['prim', "nil"]
const maintest2_div: Cion.RawLisp<"(/ 0 0)"> = ['prim', "nil"]
const maintest3_div: Cion.RawLisp<"(/ 4 4)"> = ['prim', ["0000000000000100", '0000000000000100']]
const maintest4_div: Cion.RawLisp<"(/ 4 2)"> = ['prim', ["0000000000000100", '0000000000000010']]
const maintest5_div: Cion.RawLisp<"(/ 4 (- 0 2))"> = ['prim', ['0000000000000100', "1111111111111110"]]
const maintest6_div: Cion.RawLisp<"(/ (- 2 6) (- 0 2))"> = ['prim', ["1111111111111100", "1111111111111110"]]
const maintest7_div: Cion.RawLisp<"(/ (- 2 6) -2)"> = ['prim', ["1111111111111100", "1111111111111110"]]

const maintest8_div: Cion.RawLisp<"(/ 2/3 -2)"> = ['prim', ["0000000000000010", "1111111111111010"]]
const maintest9_div: Cion.RawLisp<"(/ -2/3 -2)"> = ['prim', ["1111111111111110", "1111111111111010"]]
const maintest10_div: Cion.RawLisp<"(/ 1/9 (/ 2/3 3))"> = ['prim', ["0000000000001001", "0000000000010010"]]
