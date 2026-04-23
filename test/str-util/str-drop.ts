import type * as strutil from '../../src/strutil.js'
import type { Equal } from '../../src/util.js'

const test_strdrop_3: true = {} as Equal<'345', strutil.StrDrop<'012345', '0000000000000011'>>
const test_strdrop_0: true = {} as Equal<'012345', strutil.StrDrop<'012345', '0000000000000000'>>
const test_strdrop_overflow: true = {} as Equal<'', strutil.StrDrop<'012345', '0000000000001111'>>
