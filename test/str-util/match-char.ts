import type * as strutil from '../../src/strutil.js'
import type { Equal } from '../../src/util.js'

const test_matchchar_true: true = {} as Equal<true, strutil.MatchChar<'s', 's'>>
const test_matchchar_empty_src: true = {} as Equal<false, strutil.MatchChar<'', 's'>>
const test_matchchar_empty_tar: true = {} as Equal<false, strutil.MatchChar<'s', ''>>
const test_matchchar_multi: true = {} as Equal<false, strutil.MatchChar<'s', 'ss'>>
const test_matchchar_dot_literal: true = {} as Equal<false, strutil.MatchChar<'s', '.'>>
const test_matchchar_escaped_dot: true = {} as Equal<false, strutil.MatchChar<'s', '\\.'>>

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
