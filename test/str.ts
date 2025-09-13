import type { Str, Eval } from '../src/index'
import type Cion from '../src/index'

// test str
const evalstrtest: Eval<[[`sym`, `str`], [`prim`, `head/`], [`prim`, `tail`]]> =
  [`prim`, `'head/tail'`];

// test str
const strtest1: Str<[[`prim`, `test`], [`prim`, `+`], [`prim`, `tail`]]> = [
  `prim`,
  `'test+tail'`,
];
const strtest2: Str<[[`prim`, `test`]]> = [`prim`, `'test'`];

const lisp_strtest0: Cion.Lisp<`(str 'a' 'b')`> = "'ab'"
const lisp_strtest1: Cion.Lisp<`(str 0 1 2)`> = "'012'"
const lisp_strtest2: Cion.Lisp<`(str 0 1/1 2)`> = "'012'" // [note] `1/1` is `1` internally.
const lisp_strtest3: Cion.Lisp<`(str 0 2/3 2)`> = "'02/32'"
const lisp_strtest4: Cion.Lisp<`(str 0 nil 2)`> = "'0nil2'"
const lisp_strtest5: Cion.Lisp<`(str 0 false 2)`> = "'0false2'"
const lisp_strtest6: Cion.Lisp<`(str 0 [] 2)`> = "'0[]2'"
const lisp_strtest7: Cion.Lisp<`(str 0 {} 2)`> = "'0nil2'" // [todo] later
const lisp_strtest8: Cion.Lisp<`(str 0 [0 1] 2)`> = "'0[0 1]2'"
const lisp_strtest9: Cion.Lisp<`(str 0 {:a 0} 2)`> = "'0{:a 0}2'"
const lisp_strtest10: Cion.Lisp<`(str 0 [0 1] {:a 0} 2)`> = "'0[0 1]{:a 0}2'"






