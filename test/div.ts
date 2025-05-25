import type Cion from '../src/index.ts'

const maintest0_div: Cion.RawLisp<"(/ 4 1)"> = ['prim', "0000000000000100"]
const maintest1_div: Cion.RawLisp<"(/ 1 0)"> = ['prim', "nil"]
const maintest2_div: Cion.RawLisp<"(/ 0 0)"> = ['prim', "nil"]
const maintest3_div: Cion.RawLisp<"(/ 4 4)"> = ['prim', "0000000000000001"]
const maintest4_div: Cion.RawLisp<"(/ 4 2)"> = ['prim', "0000000000000010"]
const maintest5_div: Cion.RawLisp<"(/ 4 (- 0 2))"> = ['prim', "1111111111111110"]
const maintest6_div: Cion.RawLisp<"(/ (- 2 6) (- 0 2))"> = ['prim', "0000000000000010"]
const maintest7_div: Cion.RawLisp<"(/ (- 2 6) -2)"> = ['prim', "0000000000000010"]
