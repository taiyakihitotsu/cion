import type { Get, LispGet, Eval, IsKeyMapSexpr } from '../src/index'
import type {LetVal,LetArg,LetForm,Each,Atom,TMap,Sexpr,TNil,Keyword,Sym,PrimString,PrimBoolean,PrimTestNumber,PrimNumber,Prim,Args,Fn,Vector,Var,Env,TNotMatch,IfForm} from '../src/sexprtypes'
import {VNil,VNotMatch} from '../src/sexprtypes'

const iskeymapsexprtest0: IsKeyMapSexpr<[['key', ':a'],['key', ':b']]> = false
const iskeymapsexprtest1: IsKeyMapSexpr<[['map', [['key', ':a'], ['prim', '0']]],['map', [['key', ':a'], ['prim', '1']]]]> = false
const iskeymapsexprtest2: IsKeyMapSexpr<[['key', ':a'],['map', [['key', ':a'], ['prim', '0']]]]> = true
const iskeymapsexprtest3: IsKeyMapSexpr<[['map', [['key', ':a'], ['prim', '0']]],['key', ':a']]> = true

// test get
const evallisp_get_0: IsKeyMapSexpr<[['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]]> = true
const evallisp_get_1: Eval<[['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]]> = ['prim', '0']
const evallisp_get_2: IsKeyMapSexpr<[['map', [['key', ':a'], ['prim', '0']]],['key', ':a']]> = true
const evallisp_get_3: Eval<[['map', [['key', ':a'], ['prim', '0']]], ['key', ':a']]> = ['prim', '0']

const test_get_0: LispGet<[['map', [['key', ':a'], ['prim', '0']]], ['key', ':a']]> = ['prim', '0']
const test_get_1: LispGet<[['map', [['key', ':'], ['prim', '0']]], ['key', ':a']]> = VNil

const testget0: Get<
  ['prim', '11'],
  [`vec`, [`prim`, 0], [`prim`, 1], [`prim`, 2], [`prim`, 3], [`prim`, 4]]
> = [`prim`, 3];
const testget1: Get<['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]> = ['prim', '0']
const testget2: Get<['key', ':a'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]> = ['prim', '0']
const testget3: Get<['key', ':c'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]> = VNil
const testget4: Get<['prim', '0'], ['vec', ['key', ':a'], ['prim', '0']]> = ['key', ':a']
const testget5: Get<['prim', '1'], ['vec', ['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]> = ['prim', '10']
const testget6: Get<['prim', '111'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]> = VNil
