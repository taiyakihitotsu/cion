import type { ReduceDelEnv } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const reduce_delenv_test0:  true = {} as Equal<
  ReduceDelEnv<[['sym', 'a']], [[{name: 'a', value: 's'}]]>
, [[]]>

const reduce_delenv_test1:  true = {} as Equal<
  ReduceDelEnv<[['sym', 'a']], [[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[{name: 'b', value: 's'}]]>

const reduce_delenv_test2:  true = {} as Equal<
  ReduceDelEnv<[['sym', 'a']], [[]]>
, [[]]>

const reduce_delenv_test3:  true = {} as Equal<
  ReduceDelEnv<[['sym', 'a']], [[],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[],[{name: 'b', value: 's'}]]>

const reduce_delenv_test4:  true = {} as Equal<
  ReduceDelEnv<[['sym', 'a']], [[{name: 'a', value:'s'}],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[],[{name: 'b', value: 's'}]]>

const reduce_delenv_test5:  true = {} as Equal<
  ReduceDelEnv<[['sym', 'a']], [[{name: 'c', value:'s'}],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[{name: 'c', value:'s'}],[{name: 'b', value: 's'}]]>

const reduce_delenv_test6:  true = {} as Equal<
  ReduceDelEnv<[['sym', 'x']], [[{name: 'c', value:'s'}],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[{name: 'c', value:'s'}],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>

const reduce_delenv_test7:  true = {} as Equal<
  ReduceDelEnv<[['sym', 'a'], ['sym', 'x']], [[{name: 'c', value:'s'}],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[{name: 'c', value:'s'}],[{name: 'b', value: 's'}]]>

const reduce_delenv_test:  true = {} as Equal<
  ReduceDelEnv<[['sym', 'a'], ['sym', 'b'], ['sym', 'c']], [[{name: 'c', value:'s'}],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[], []]>
