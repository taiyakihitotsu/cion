import type * as strutil from '../../src/strutil.js'
import type { Equal } from '../../src/util.js'

const test_strlen_3: true = {} as Equal<'0000000000000011', strutil.StrLen<'111'>>
const test_strlen_0: true = {} as Equal<'0000000000000000', strutil.StrLen<''>>
