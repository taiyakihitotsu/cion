import type { GetVar, MakeVar } from '../../src/index.js'
import {VNotMatch} from '../../src/sexprtypes.js'
import type { Equal } from '../../src/util.js'

const getVarTest: true = {} as Equal<
  GetVar<"s", [MakeVar<"ss", "stringer">, MakeVar<"s", "string">]>
, "string">

const getVarTest2: true = {} as Equal<
  GetVar<"ss", [MakeVar<"ss", "stringer">, MakeVar<"s", "string">]>
, "stringer">

const EvalTest3: true = {} as Equal<
  GetVar<"sss", [MakeVar<"ss", "stringer">, MakeVar<"s", "string">]>
, typeof VNotMatch>
