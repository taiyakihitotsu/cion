import type Cion from '../src/index'
import type { LispRelation, Eval } from '../src/index'

const jkkjkt: Eval<[['sym', 'not'], [['sym', '>'], ['prim', '0000000000000010'], ['prim', '00000000000000001']]]> = ['prim', false]

const testlispgt0: LispRelation<'>',  [[`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, true]
const testlispgt1: LispRelation<'>',  [[`prim`, '00001111'], [`prim`, '00000011'], [`prim`, '0000001']]> = [`prim`, true]
const testlispgt2: LispRelation<'>',  [[`prim`, '00001111'], [`prim`, '00001111'], [`prim`, '00001111']]> = [`prim`, false]

const maintest11_gt_0: Cion.RawLisp<"(> 15 14)"> = [`prim`, true]
const maintest11_gt_1: Cion.RawLisp<"(> 15 14 13)"> = [`prim`, true]
const maintest11_gt_2: Cion.RawLisp<"(> 15 14 16)"> = [`prim`, false]
