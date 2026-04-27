import type { Eval, MakeVar } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// Returns evaluated symbol 

const evalatomtest: true = {} as Equal<Eval<[`prim`, `'test'`]>, [`prim`, `'test'`]>
const evalatomtest2: true = {} as Equal<Eval<[`sym`, `test`], [[MakeVar<`test`, `'testval'`>]]>, [
  `prim`,
  `'testval'`,
]>
const evalatomtest3: true = {} as Equal<Eval<
  [`sym`, `test`],
  [[MakeVar<`test`, [`prim`, `'prim/test'`]>]]
>, [`prim`, `'prim/test'`]>
const evalatomtest4: true = {} as Equal<Eval<
  [`sym`, `test`],
  [[MakeVar<`test`, [`fn`, [[`sym`, `a`]], [`sym`, `a`]]>]]
>, [`fn`, [[`sym`, `a`]], [`sym`, `a`]]>

// Return primitive

const evalprimerrortest: true = {} as Equal<Eval<[`prim`, 0]>, [`prim`, 0]>
