import type { GetMap } from '../../src/index.js'
import type { Equal } from '../../src/util.js'
import type { TNil } from '../../src/sexprtypes.js'

const testgetmap0: true = {} as Equal<GetMap<['key', ':a'], ['map', [['key', ':a'], ['prim', '0']]]>, ['prim', '0']>

type Actual_getmap_success = GetMap<['key', ':a'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]>
const testgetmap1: true = {} as Equal<['prim', '0'], Actual_getmap_success>

type Actual_getmap_not_exist = GetMap<['key', ':c'], ['map', [['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]]>
const testgetmap2: true = {} as Equal<TNil, Actual_getmap_not_exist>

type Actual_getmap_vec = GetMap<['key', ':c'], ['vec', ['key', ':b'], ['prim', '10'], ['key', ':a'], ['prim', '0']]>
const testgetmap3: true = {} as Equal<never, Actual_getmap_vec>

type Actual_getmap_error_emptymap = GetMap<['key', ':a'], ['map']>
const testgetmap4: true = {} as Equal<never, Actual_getmap_error_emptymap>

type Actual_getmap_emptymap = GetMap<['key', ':a'], ['map', []]>
const testgetmap5: true = {} as Equal<TNil, Actual_getmap_emptymap>
