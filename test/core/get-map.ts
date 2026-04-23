import type { GetMap } from '../../src/index.js'
import type { Equal } from '../../src/util.js'
import type { TNil } from '../../src/sexprtypes.js'

const testgetmap0: true = {} as Equal<
GetMap<['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]>, ['prim', '0']>

const testgetmap1: true = {} as Equal<
GetMap<['key', ':a'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]>, ['prim', '0']>

const testgetmap2: true = {} as Equal<
  GetMap<['key', ':c'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]>, TNil>
