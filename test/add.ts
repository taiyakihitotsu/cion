import type Cion from '../src/index'
import type { LispAdd } from '../src/index'

const testlispadd0: LispAdd<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, '0000000000000100']
const testlispadd1: LispAdd<[[`prim`, '00000011'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, '0000000000000111']
const testlispadd2: LispAdd<[[`prim`, '00001001'], [`prim`, '00000110'], [`prim`, '00000001']]> = [`prim`, '0000000000010000']

const maintest0_add: Cion.RawLisp<"(+ 1 1)"> = ['prim', "0000000000000010"]
const maintest1_add: Cion.RawLisp<"(+ 1 0)"> = ['prim', "0000000000000001"]
const maintest2_add: Cion.RawLisp<"(+ 0 0)"> = ['prim', "0000000000000000"]
const maintest3_add: Cion.RawLisp<"(+ 4 5)"> = ['prim', "0000000000001001"]
const maintest4_add: Cion.RawLisp<"(+ 5 4 6)"> = ['prim', "0000000000001111"]
const maintest5_add: Cion.RawLisp<"(+ 5 4 0)"> = ['prim', "0000000000001001"]
const maintest6_add: Cion.RawLisp<"(+ 5 0 4)"> = ['prim', "0000000000001001"]
const maintest7_add: Cion.RawLisp<"(+ 0 4 5)"> = ['prim', "0000000000001001"]
const maintest8_add: Cion.RawLisp<"(+ -1 1)"> = ['prim', "0000000000000000"]
const maintest9_add: Cion.RawLisp<"(+ -2 -2)"> = ['prim', "1111111111111100"]
