import type Cion from '../../src/index'
import type { LispRelation, Eval } from '../../src/index'
import type {Equal} from '../../src/util'

const gt_test_0 : true = {} as Equal<['prim', false], 
  Eval<[['sym', 'not'], [['sym', '>'], ['prim', '0000000000000010'], ['prim', '00000000000000001']]]>>

const gt_test_1 : true = {} as Equal<[`prim`, true], 
  LispRelation<'>', [[`prim`, '00000011'], [`prim`, '0000001']]>>

const gt_test_2 : true = {} as Equal<[`prim`, true], 
  LispRelation<'>', [[`prim`, '00001111'], [`prim`, '00000011'], [`prim`, '0000001']]>>

const gt_test_3 : true = {} as Equal<[`prim`, false], 
  LispRelation<'>', [[`prim`, '00001111'], [`prim`, '00001111'], [`prim`, '00001111']]>>

const gt_test_4 : true = {} as Equal<[`prim`, true], 
  Cion.RawLisp<"(> 15 14)">>

const gt_test_5 : true = {} as Equal<[`prim`, true], 
  Cion.RawLisp<"(> 15 14 13)">>

const gt_test_6 : true = {} as Equal<[`prim`, false], 
  Cion.RawLisp<"(> 15 14 16)">>
