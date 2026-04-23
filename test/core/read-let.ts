import type { Let, ReadLet, MakeVar } from '../../src/index.js'
import type { Equal } from '../../src/util.js'
import {VNotMatch} from '../../src/sexprtypes.js'

export type LetEnvLifo =
[
  [
    MakeVar<"ss", "stringer">,
    MakeVar<"s", "string">,
    MakeVar<"cc", [`prim`, "p/cc"]>,
  ],
]

const letTest: true = {} as Equal<
Let<"sss", "str", LetEnvLifo>, [
  [
    { name: "ss", value: "stringer" },
    { name: "s", value: "string" },
    { name: "cc", value: [`prim`, `p/cc`] },
  ],
  [{ name: "sss", value: "str" }],
]>

const readLetTest:   true = {} as Equal<
ReadLet<"s", Let<"sss", "str", LetEnvLifo>>, "string">

const readLetTest2:   true = {} as Equal<
ReadLet<"sss", Let<"sss", "str", LetEnvLifo>>, "str">

const readLetTest3:   true = {} as Equal<
ReadLet<"ssss", Let<"sss", "str", LetEnvLifo>>, typeof VNotMatch>

const readLetTest4:   true = {} as Equal<
ReadLet<
  "sss",
  Let<"sss", [`prim`, `p/sss`], LetEnvLifo>
>, [`prim`, `p/sss`]>

const readLetTest5:   true = {} as Equal<
ReadLet<"cc", Let<"sss", "str", LetEnvLifo>>, [
  `prim`,
  `p/cc`,
]>
