import type * as strutil from '../../src/strutil.js'
import type { Equal } from '../../src/util.js'

const test_searchhead_success: true = {} as Equal<['s', 'ssss'], strutil.StrSearchHead<'sssss', 's'>>
const test_searchhead_fail_pos: true = {} as Equal<[], strutil.StrSearchHead<'sssxx', 'xx'>>
const test_searchhead_match_start: true = {} as Equal<['xx', 'sss'], strutil.StrSearchHead<'xxsss', 'xx'>>
const test_searchhead_no_match: true = {} as Equal<[], strutil.StrSearchHead<'xxsss', 'd'>>
const test_searchhead_dot: true = {} as Equal<['x', 'xsss'], strutil.StrSearchHead<'xxsss', '.'>>
const test_searchhead_escaped_dot: true = {} as Equal<[], strutil.StrSearchHead<'xxsss', '\\.'>>

// --- StrSearchHead (Advanced patterns with dots and anchors) ---
const test_search_head_complex_f: true = {} as Equal<[], strutil.StrSearchHead<'xxsss', 'xs'>>
const test_search_head_complex_t: true = {} as Equal<['xs', 'ss'], strutil.StrSearchHead<'xsss', 'xs'>>
const test_search_head_dot_t: true = {} as Equal<['ss', 'sss'], strutil.StrSearchHead<'sssss', '.s'>>
const test_search_head_dot_f: true = {} as Equal<[], strutil.StrSearchHead<'sssxx', '..xx'>>
const test_search_head_dot_t2: true = {} as Equal<['sssxx', ''], strutil.StrSearchHead<'sssxx', '...xx'>>
const test_search_head_dot_f2: true = {} as Equal<[], strutil.StrSearchHead<'sssxx', '.xx'>>
const test_search_head_mixed: true = {} as Equal<['xxs', 'ss'], strutil.StrSearchHead<'xxsss', 'xx.'>>
const test_search_head_mixed_f: true = {} as Equal<[], strutil.StrSearchHead<'xxsss', '.d'>>
const test_search_head_mixed2: true = {} as Equal<['xxs', 'ss'], strutil.StrSearchHead<'xxsss', 'x.s'>>
const test_search_head_mixed3: true = {} as Equal<['xxs', 'ss'], strutil.StrSearchHead<'xxsss', '.xs'>>
const test_search_head_overflow: true = {} as Equal<['xsss', ''], strutil.StrSearchHead<'xsss', 'xs..'>>
const test_search_head_unicode: true = {} as Equal<['x🦊sss', ''], strutil.StrSearchHead<'x🦊sss', 'x🦊s..'>>
