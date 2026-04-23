import type { Let, ReadAtom, MakeVar } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

export type LetEnvLifo =
[
  [
    MakeVar<"ss", "stringer">,
    MakeVar<"s", "string">,
    MakeVar<"cc", [`prim`, "p/cc"]>,
  ],
]

const readatomtest: true = {} as Equal<
ReadAtom<
  [`sym`, `sss`],
  Let<"sss", [`prim`, `p/sss`], LetEnvLifo>
>, [`prim`, `p/sss`]>

const readatomtest2: true = {} as Equal<
ReadAtom<
  [`prim`, `'sss'`],
  Let<"sss", [`prim`, `p/sss`], LetEnvLifo>
>, [`prim`, `'sss'`]>

