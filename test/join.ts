import Cion, {Join, GetStr, LispJoin } from '../src/index'

const jointest_0: Join<',', ['a', 'b', 'c']> = 'a,b,c'
const jointest_1: Join<',', ['a']> = 'a'
const jointest_2: Join<',', ['']> = ''
const jointest_3__: GetStr<['prim', 'a']> = 'a'
const jointest_3: Join<',', [['prim', 'a'], 'b', 'c']> = 'a,b,c'
const jointest_4: Join<',', [['prim', "'a'"], 'b', 'c']> = 'a,b,c'

const _lispjointest_0: LispJoin<[['prim', ','], ['vec', ['prim', `'a'`], ['prim', `'b'`]]]> = ['prim', "'a,b'"]
const _lispjointest_1: LispJoin<[['prim', ','], ['vec', ['prim', "'a'"], ['prim', `'b'`]]]> = ['prim', "'a,b'"]

const lispjointest_0: Cion.Lisp<`(join ',' ['a' 'b'])`> = `'a,b'`
// [todo]
// const lispjointest_1: Cion.Lisp<`(join ',' ["a" 'b' 'c'])`> = `'a,b,c'`
const lispjointest_1: Cion.Lisp<`(join ',' ['a' 'b' 'c'])`> = `'a,b,c'`
const lispjointest_2: Cion.Lisp<`(join ',' ['a'])`> = `'a'`
const lispjointest_3: Cion.Lisp<`(join ',' [])`> = `''`
const lispjointest_4: Cion.Lisp<`(join ',' [1 2])`> = `'1,2'`
const lispjointest_5: Cion.Lisp<`(join ',' [1 2 true])`> = `'1,2,true'`


