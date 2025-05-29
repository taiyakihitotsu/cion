import type Cion from '../src/index'
import type { LispMul } from '../src/index'

const testlispmul0: LispMul<[[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, '0000000000000011']
const testlispmul1: LispMul<[[`prim`, '00001111'], [`prim`, '0000001'], [`prim`, '00000011']]> = [`prim`, '0000000000101101']

const maintest0_mul: Cion.RawLisp<"(* 3 1)"> = ['prim', "0000000000000011"]
const maintest1_mul: Cion.RawLisp<"(* 0 1)"> = ['prim', "0000000000000000"]
const maintest2_mul: Cion.RawLisp<"(* 0 0)"> = ['prim', "0000000000000000"]
const maintest3_mul: Cion.RawLisp<"(* 2 3)"> = ['prim', "0000000000000110"]
const maintest4_mul: Cion.RawLisp<"(* (- 2 3) 3)"> = ['prim', "1111111111111101"]
const maintest5_mul: Cion.RawLisp<"(* (- 0 2) 3)"> = ['prim', "1111111111111010"]
const maintest6_mul: Cion.RawLisp<"(* -2 3)"> = ['prim', "1111111111111010"]
