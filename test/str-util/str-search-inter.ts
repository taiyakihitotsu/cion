import type * as strutil from '../../src/strutil.js'
import type { Equal } from '../../src/util.js'

// --- StrInter (Substring via Indices) ---
const test_str_inter_normal: true = {} as Equal<'2345', strutil.StrInter<'0123456789', '0000000000000010', '0000000000000101'>>
const test_str_inter_from_start: true = {} as Equal<'012345', strutil.StrInter<'0123456789', '0000000000000000', '0000000000000101'>>
const test_str_inter_to_end: true = {} as Equal<'23456789', strutil.StrInter<'0123456789', '0000000000000010', '0000000000001111'>>
