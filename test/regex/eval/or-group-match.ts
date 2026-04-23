import type { OrGroupMatch, AstaMatch, Unmatch } from '../../../src/regex/regex-eval.js'
import type { Equal } from '../../../src/util.js'

type Expected_orgroup_match = OrGroupMatch<'abc', [['a'], ['b']], ''>
const expected_orgroup_match: true = {} as Equal<['a', 'bc', []], Expected_orgroup_match>

type Expected_orgroup_first_priority = OrGroupMatch<'apple', [['app'], ['apple']], ''>
const expected_orgroup_first_priority: true = {} as Equal<['app', 'le', []], Expected_orgroup_first_priority>

type Expected_orgroup_unmatch = OrGroupMatch<'abc', [['x'], ['y']], ''>
const expected_orgroup_unmatch: true = {} as Equal<Unmatch, Expected_orgroup_unmatch>
