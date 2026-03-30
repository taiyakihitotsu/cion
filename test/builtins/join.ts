import Cion, { Join, GetStr, LispJoin } from '../../src/index'
import type { Equal } from '../../src/util'

const join_test_0 : true = {} as Equal<'a,b,c', Join<',', ['a', 'b', 'c']>>
const join_test_1 : true = {} as Equal<'a',     Join<',', ['a']>>
const join_test_2 : true = {} as Equal<'',      Join<',', ['']>>
const join_test_3 : true = {} as Equal<'a',     GetStr<['prim', 'a']>>
const join_test_4 : true = {} as Equal<'a,b,c', Join<',', [['prim', 'a'], 'b', 'c']>>
const join_test_5 : true = {} as Equal<'a,b,c', Join<',', [['prim', "'a'"], 'b', 'c']>>

const lisp_join_test_0 : true =
  {} as Equal<['prim', "'a,b'"], LispJoin<[['prim', ','], ['vec', ['prim', `'a'`], ['prim', `'b'`]]]>>

const lisp_join_test_1 : true =
  {} as Equal<['prim', "'a,b'"], LispJoin<[['prim', ','], ['vec', ['prim', "'a'"], ['prim', `'b'`]]]>>

const lisp_eval_join_test_0 : true =
  {} as Equal<`'a,b'`, Cion.Lisp<`(join ',' ['a' 'b'])`>>

const lisp_eval_join_test_1 : true =
  {} as Equal<`'a,b,c'`, Cion.Lisp<`(join ',' ['a' 'b' 'c'])`>>

const lisp_eval_join_test_2 : true =
  {} as Equal<`'a'`, Cion.Lisp<`(join ',' ['a'])`>>

const lisp_eval_join_test_3 : true =
  {} as Equal<`''`, Cion.Lisp<`(join ',' [])`>>

const lisp_eval_join_test_4 : true =
  {} as Equal<`'1,2'`, Cion.Lisp<`(join ',' [1 2])`>>

const lisp_eval_join_test_5 : true =
  {} as Equal<`'1,2,true'`, Cion.Lisp<`(join ',' [1 2 true])`>>

const lisp_eval_join_test_6 : true =
  {} as Equal<`'0,1,true,2'`, Cion.Lisp<`(join ',' [0 1 'true' 2])`>>
