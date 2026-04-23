import type { InsertLast } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

const insertlasttest0: true = {} as Equal<
InsertLast<[0,1,2,3], 'x'>, [0,1,2,3,'x']>

const insertlasttest1: true = {} as Equal<
InsertLast<[0], 'x'>, [0,'x']>

const insertlasttest2: true = {} as Equal<
InsertLast<[], 'x'>, ['x']>
