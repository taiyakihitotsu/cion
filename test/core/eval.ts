import type { Eval, MakeVar } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// [Note]
// These tests are complement tests.
//
// Main test, e.g. for built-in functions,
//   should written in `test/builtints`
//   or a particular file.
//
// 

const evalatomtest: Eval<[`prim`, `'test'`]> = [`prim`, `'test'`];
const evalatomtest2: Eval<[`sym`, `test`], [[MakeVar<`test`, `'testval'`>]]> = [
  `prim`,
  `'testval'`,
];
const evalatomtest3: Eval<
  [`sym`, `test`],
  [[MakeVar<`test`, [`prim`, `'prim/test'`]>]]
> = [`prim`, `'prim/test'`];
const evalatomtest4: Eval<
  [`sym`, `test`],
  [[MakeVar<`test`, [`fn`, [[`sym`, `a`]], [`sym`, `a`]]>]]
> = [`fn`, [[`sym`, `a`]], [`sym`, `a`]];
const evalprimerrortest: Eval<[`prim`, 0]> = [`prim`, 0];
