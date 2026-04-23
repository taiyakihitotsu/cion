import type { OrMatch, Unmatch } from '../../../src/regex/regex-eval.js'
import type { Equal } from '../../../src/util.js'

type Expected_basic_match = OrMatch<'abc', ['a', 'b'], ''>
const expected_basic_match: true = {} as Equal<['a', 'bc'], Expected_basic_match>

type Expected_first_match = OrMatch<'apple juice', ['apple', 'app'], ''>
const expected_first_match: true = {} as Equal<['apple', ' juice'], Expected_first_match>

type Expected_with_forward = OrMatch<'world', ['world'], 'hello '>
const expected_with_forward: true = {} as Equal<['hello world', ''], Expected_with_forward>

type Expected_unmatch = OrMatch<'bytecode', ['code', 'data'], ''>
const expected_unmatch: true = {} as Equal<Unmatch, Expected_unmatch>

type Expected_empty_list = OrMatch<'anything', [], ''>
const expected_empty_list: true = {} as Equal<Unmatch, Expected_empty_list>

type Expected_order_priority = OrMatch<'test', ['t', 'te', 'test'], ''>
const expected_order_priority: true = {} as Equal<['t', 'est'], Expected_order_priority>
