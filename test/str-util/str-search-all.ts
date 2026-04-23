import type * as strutil from '../../src/strutil.js'
import type { Equal } from '../../src/util.js'

// --- StrSearchAll (Global vs Anchored Search) ---
const test_search_all_greedy: true = {} as Equal<['s', 'ssss'], strutil.StrSearchAll<'sssss', 's'>>
const test_search_all_found: true = {} as Equal<['sssxx', ''], strutil.StrSearchAll<'sssxx', 'xx'>>
const test_search_all_start: true = {} as Equal<['xx', 'sss'], strutil.StrSearchAll<'xxsss', 'xx'>>
const test_search_all_overlap: true = {} as Equal<['xx', 'xsss'], strutil.StrSearchAll<'xxxsss', 'xx'>>
const test_search_all_middle: true = {} as Equal<['ssxx', 'sss'], strutil.StrSearchAll<'ssxxsss', 'xx'>>
const test_search_all_none: true = {} as Equal<[], strutil.StrSearchAll<'xxsss', 'd'>>

// StrSearchAll with End-Anchor ($)
const test_search_all_end_t: true = {} as Equal<['sssss', ''], strutil.StrSearchAll<'sssss', 's', '$'>>
const test_search_all_end_t2: true = {} as Equal<['sssxx', ''], strutil.StrSearchAll<'sssxx', 'xx', '$'>>
const test_search_all_end_f: true = {} as Equal<[], strutil.StrSearchAll<'xxsss', 'xx', '$'>>
const test_search_all_end_f2: true = {} as Equal<[], strutil.StrSearchAll<'ssxxsss', 'xx', '$'>>
const test_search_all_end_none: true = {} as Equal<[], strutil.StrSearchAll<'xxsss', 'd', '$'>>

// StrSearchAll with Start-Anchor (^)
const test_search_all_start_t: true = {} as Equal<['s', 'ssss'], strutil.StrSearchAll<'sssss', 's', '^'>>
const test_search_all_start_f: true = {} as Equal<[], strutil.StrSearchAll<'sssxx', 'xx', '^'>>
const test_search_all_start_match: true = {} as Equal<['xx', 'sss'], strutil.StrSearchAll<'xxsss', 'xx', '^'>>
const test_search_all_start_none: true = {} as Equal<[], strutil.StrSearchAll<'xxsss', 'd', '^'>>
