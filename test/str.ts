import type { Str, Eval } from '../src/index'

// test str
const evalstrtest: Eval<[[`sym`, `str`], [`prim`, `head/`], [`prim`, `tail`]]> =
  [`prim`, `'head/tail'`];

// test str
const strtest1: Str<[[`prim`, `test`], [`prim`, `+`], [`prim`, `tail`]]> = [
  `prim`,
  `'test+tail'`,
];
const strtest2: Str<[[`prim`, `test`]]> = [`prim`, `'test'`];
