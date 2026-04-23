import type { InsertSecond } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const insert2ndtest0: true = {} as Equal<
InsertSecond<[0,1,2,3], 'x'>
, [0,'x',1,2,3]>

const insert2ndtest1: true = {} as Equal<
InsertSecond<[0], 'x'>, [0,'x']>

const insert2ndtest2: true = {} as Equal<
InsertSecond<[], 'x'>, ['x']>

const insert2ndtest3: true = {} as Equal<InsertSecond<[], ['a', 'x']>, [['a', 'x']]>

const insert2ndtest4: true = {} as Equal<InsertSecond<['b', 'y'], ['a', 'x']>, ['b', ['a', 'x'], 'y']>

const insert2ndtest5: true = {} as Equal<InsertSecond<[['b', 'y']], ['a', 'x']>, [['b', 'y'], ['a', 'x']]>
