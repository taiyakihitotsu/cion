import type * as strutil from '../../src/strutil.js'
import type { Equal } from '../../src/util.js'

const test_charat_match: true = {} as Equal<'2', strutil.CharAt<'123', '0000000000000001'>>
const test_charat_out_of_range: true = {} as Equal<'', strutil.CharAt<'123', '0000000000000111'>>
const test_charat_empty: true = {} as Equal<'', strutil.CharAt<'', '0000000000000001'>>
