import type Cion from '../src/index.ts'

const maintest0_mul: Cion.RawLisp<"(* 3 1)"> = ['prim', "0000000000000011"]
const maintest1_mul: Cion.RawLisp<"(* 0 1)"> = ['prim', "0000000000000000"]
const maintest2_mul: Cion.RawLisp<"(* 0 0)"> = ['prim', "0000000000000000"]
const maintest3_mul: Cion.RawLisp<"(* 2 3)"> = ['prim', "0000000000000110"]
const maintest4_mul: Cion.RawLisp<"(* (- 2 3) 3)"> = ['prim', "1111111111111101"]
const maintest5_mul: Cion.RawLisp<"(* (- 0 2) 3)"> = ['prim', "1111111111111010"]
const maintest6_mul: Cion.RawLisp<"(* -2 3)"> = ['prim', "1111111111111010"]
