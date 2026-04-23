import type * as strutil from '../../src/strutil.js'
import type { Equal } from '../../src/util.js'

const test_strtake_3: true = {} as Equal<'012', strutil.StrTake<'012345', '0000000000000011'>>
const test_strtake_0: true = {} as Equal<'', strutil.StrTake<'012345', '0000000000000000'>>
const test_strtake_overflow: true = {} as Equal<'012345', strutil.StrTake<'012345', '0000000000001111'>>

