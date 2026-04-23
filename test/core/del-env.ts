import type { DelEnv } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const delenv_test0: true = {} as Equal<
  DelEnv<'a', [[{name: 'a', value: 's'}]]>
, [[]]>

const delenv_test1: true = {} as Equal<
  DelEnv<'a', [[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[{name: 'b', value: 's'}]]>

const delenv_test2: true = {} as Equal<
  DelEnv<'a', [[]]>
, [[]]>

const delenv_test3: true = {} as Equal<
  DelEnv<'a', [[],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[],[{name: 'b', value: 's'}]]>

const delenv_test4: true = {} as Equal<
  DelEnv<'a', [[{name: 'a', value:'s'}],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[],[{name: 'b', value: 's'}]]>

const delenv_test5: true = {} as Equal<
  DelEnv<'a', [[{name: 'c', value:'s'}],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[{name: 'c', value:'s'}],[{name: 'b', value: 's'}]]>

const delenv_test: true = {} as Equal<
  DelEnv<'x', [[{name: 'c', value:'s'}],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
, [[{name: 'c', value:'s'}],[{name: 'a', value: 's'}, {name: 'b', value: 's'}]]>
