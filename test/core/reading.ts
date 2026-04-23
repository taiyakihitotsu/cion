import type { Reading, MakeVar } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const readingtest0: true = {} as Equal<
  Reading<
  [[`sym`, `a`], [`sym`, `b`], [`prim`, `c-str`]],
  [[], [MakeVar<"a", [`prim`, "a-str"]>, MakeVar<"b", [`prim`, "b-str"]>]]
>, [
  ["prim", "a-str"],
  ["prim", "b-str"],
  ["prim", "c-str"],
]>

const readingtest1: true = {} as Equal<
Reading<
  [['sym', 'a']],
  [[]]
>,
  { sexpr: ["NotMatch"]
    , error: 'ReadingError0'
    , message: 'sexpr is not atom list.'
  }>

const readingtest2: true = {} as Equal<
Reading<
[['sym', 'a'], ['sym', 'b'], [['sym', 'str'], ['prim', "'s1'"], ['prim', "'s2'"]]],
[[],
 [MakeVar<"a", ['sym', 'str']>, 
  MakeVar<'b', ['prim', "'bs'"]>]]>, [['sym', 'str'], ['prim', "'bs'"], ['prim', "'s1s2'"]]>
