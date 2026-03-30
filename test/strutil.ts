import type * as strutil from '../src/strutil'
import type { Equal } from '../src/util'

// --- String Utility Patterns ---

// RegCut
const test_regcut_0: true = {} as Equal<['{', '1,}))rest'], strutil.RegCut<'{1,}))rest'>>

// StrLen (Fixed-length binary representation)
const test_strlen_3: true = {} as Equal<'0000000000000011', strutil.StrLen<'111'>>
const test_strlen_0: true = {} as Equal<'0000000000000000', strutil.StrLen<''>>

// CharAt
const test_charat_match: true = {} as Equal<'2', strutil.CharAt<'123', '0000000000000001'>>
const test_charat_out_of_range: true = {} as Equal<'', strutil.CharAt<'123', '0000000000000111'>>
const test_charat_empty: true = {} as Equal<'', strutil.CharAt<'', '0000000000000001'>>

// MatchChar
const test_matchchar_true: true = {} as Equal<true, strutil.MatchChar<'s', 's'>>
const test_matchchar_empty_src: true = {} as Equal<false, strutil.MatchChar<'', 's'>>
const test_matchchar_empty_tar: true = {} as Equal<false, strutil.MatchChar<'s', ''>>
const test_matchchar_multi: true = {} as Equal<false, strutil.MatchChar<'s', 'ss'>>
const test_matchchar_dot_literal: true = {} as Equal<false, strutil.MatchChar<'s', '.'>>
const test_matchchar_escaped_dot: true = {} as Equal<false, strutil.MatchChar<'s', '\\.'>>

// SomeLen (Length comparison/equality)
const test_somelen_same: true = {} as Equal<true, strutil.SomeLen<'sss', 'xxa'>>
const test_somelen_diff: true = {} as Equal<false, strutil.SomeLen<'sss', 'sxxa'>>
const test_somelen_tar_empty: true = {} as Equal<false, strutil.SomeLen<'sss', ''>>
const test_somelen_src_empty: true = {} as Equal<false, strutil.SomeLen<'', 'xxa'>>
const test_somelen_both_empty: true = {} as Equal<true, strutil.SomeLen<'', ''>>

// StrTake
const test_strtake_3: true = {} as Equal<'012', strutil.StrTake<'012345', '0000000000000011'>>
const test_strtake_0: true = {} as Equal<'', strutil.StrTake<'012345', '0000000000000000'>>
const test_strtake_overflow: true = {} as Equal<'012345', strutil.StrTake<'012345', '0000000000001111'>>

// StrDrop
const test_strdrop_3: true = {} as Equal<'345', strutil.StrDrop<'012345', '0000000000000011'>>
const test_strdrop_0: true = {} as Equal<'012345', strutil.StrDrop<'012345', '0000000000000000'>>
const test_strdrop_overflow: true = {} as Equal<'', strutil.StrDrop<'012345', '0000000000001111'>>

// StrSearchHead
const test_searchhead_success: true = {} as Equal<['s', 'ssss'], strutil.StrSearchHead<'sssss', 's'>>
const test_searchhead_fail_pos: true = {} as Equal<[], strutil.StrSearchHead<'sssxx', 'xx'>>
const test_searchhead_match_start: true = {} as Equal<['xx', 'sss'], strutil.StrSearchHead<'xxsss', 'xx'>>
const test_searchhead_no_match: true = {} as Equal<[], strutil.StrSearchHead<'xxsss', 'd'>>
const test_searchhead_dot: true = {} as Equal<['x', 'xsss'], strutil.StrSearchHead<'xxsss', '.'>>
const test_searchhead_escaped_dot: true = {} as Equal<[], strutil.StrSearchHead<'xxsss', '\\.'>>

// --- Character Class Matchers (\d, \w, \u, \l and their negations) ---
const test_match_digit_t: true = {} as Equal<true, strutil.MatchChar<'1', '\\d'>>
const test_match_digit_t2: true = {} as Equal<true, strutil.MatchChar<'0', '\\d'>>
const test_match_digit_f: true = {} as Equal<false, strutil.MatchChar<'s', '\\d'>>

const test_match_word_t: true = {} as Equal<true, strutil.MatchChar<'1', '\\w'>>
const test_match_word_t2: true = {} as Equal<true, strutil.MatchChar<'a', '\\w'>>
const test_match_word_f: true = {} as Equal<false, strutil.MatchChar<'!', '\\w'>>

const test_match_upper_f: true = {} as Equal<false, strutil.MatchChar<'1', '\\u'>>
const test_match_upper_f2: true = {} as Equal<false, strutil.MatchChar<'d', '\\u'>>
const test_match_upper_t: true = {} as Equal<true, strutil.MatchChar<'D', '\\u'>>

const test_match_lower_f: true = {} as Equal<false, strutil.MatchChar<'1', '\\l'>>
const test_match_lower_t: true = {} as Equal<true, strutil.MatchChar<'d', '\\l'>>
const test_match_lower_f2: true = {} as Equal<false, strutil.MatchChar<'D', '\\l'>>

const test_match_not_digit_f: true = {} as Equal<false, strutil.MatchChar<'1', '\\D'>>
const test_match_not_digit_f2: true = {} as Equal<false, strutil.MatchChar<'0', '\\D'>>
const test_match_not_digit_t: true = {} as Equal<true, strutil.MatchChar<'s', '\\D'>>

const test_match_not_word_f: true = {} as Equal<false, strutil.MatchChar<'1', '\\W'>>
const test_match_not_word_f2: true = {} as Equal<false, strutil.MatchChar<'a', '\\W'>>
const test_match_not_word_t: true = {} as Equal<true, strutil.MatchChar<'!', '\\W'>>

const test_match_not_upper_t: true = {} as Equal<true, strutil.MatchChar<'1', '\\U'>>
const test_match_not_upper_t2: true = {} as Equal<true, strutil.MatchChar<'d', '\\U'>>
const test_match_not_upper_f: true = {} as Equal<false, strutil.MatchChar<'D', '\\U'>>

const test_match_not_lower_t: true = {} as Equal<true, strutil.MatchChar<'1', '\\L'>>
const test_match_not_lower_t2: true = {} as Equal<true, strutil.MatchChar<'d', '\\L'>>
const test_match_not_lower_f: true = {} as Equal<false, strutil.MatchChar<'D', '\\L'>>

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

// --- StrInter (Substring via Indices) ---
const test_str_inter_normal: true = {} as Equal<'2345', strutil.StrInter<'0123456789', '0000000000000010', '0000000000000101'>>
const test_str_inter_from_start: true = {} as Equal<'012345', strutil.StrInter<'0123456789', '0000000000000000', '0000000000000101'>>
const test_str_inter_to_end: true = {} as Equal<'23456789', strutil.StrInter<'0123456789', '0000000000000010', '0000000000001111'>>
