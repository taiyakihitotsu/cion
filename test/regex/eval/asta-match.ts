import type { AstaMatch } from '../../../src/regex/regex-eval.js'
import type { Equal } from '../../../src/util.js'

type Expected_asta_zero = AstaMatch<'abc', ['x'], ''>
const expected_asta_zero: true = {} as Equal<['', 'abc'], Expected_asta_zero>

type Expected_asta_multiple = AstaMatch<'xxxabc', ['x'], ''>
const expected_asta_multiple: true = {} as Equal<['xxx', 'abc'], Expected_asta_multiple>

type Expected_asta_group = AstaMatch<'ababab', [['a', 'b']], ''>
const expected_asta_group: true = {} as Equal<['ababab', ''], Expected_asta_group>

type Expected_asta_full_match = AstaMatch<'aaaaa', ['a'], ''>
const expected_asta_full_match: true = {} as Equal<['aaaaa', ''], Expected_asta_full_match>

type Expected_asta_with_forward = AstaMatch<'bbb', ['b'], 'aaa'>
const expected_asta_with_forward: true = {} as Equal<['aaabbb', ''], Expected_asta_with_forward>

type Expected_asta_partial_failure = AstaMatch<'axaxab', ['a', 'x'], ''>
// @ts-expect-error: `Or` AST is passed as [["a", "b"]], NOT ["a", "b"].
const expected_asta_partial_failure: true = {} as Equal<['axaxa', 'b'], Expected_asta_partial_failure>
