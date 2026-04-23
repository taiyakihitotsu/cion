import type { Let, ReadLetRecur, MakeVar } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

export type LetEnvLifo =
[
  [
    MakeVar<"ss", "stringer">,
    MakeVar<"s", "string">,
    MakeVar<"cc", [`prim`, "p/cc"]>,
  ],
]

const readletrecur_test0: true = {} as Equal<
ReadLetRecur<['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]], Let<'b', ['prim', '0010'], Let<'a', ['prim', '0001'], LetEnvLifo>>>, 
['fn', [['sym', 'x']], [['sym', '+'], ['prim', '0001'], ['prim', '0010']]]>
