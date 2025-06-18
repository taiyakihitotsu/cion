import type { Util as u} from '../src/util'


const test_dissoc0: u.DissocKeys<{a: 1, b: 2}, ['b']> = {a: 1}
// @ts-expect-error:
const test_dissocb: u.DissocKeys<{a: 1, b: 2}, ['b']> = {b: 1}
const test_dissoc: u.DissocKeys<{a: 1, b: 2}, ['b', 'a']> = {}

const test_utot: u.UtoT<1 | 2> extends [(1|2), (1|2)] ? [1,2] : never = [1,2]
const test_ks: u.KeysTuple<{a: 1, b: 2}> extends [('b'|'a'), ('b'|'a')] ? ['b','a'] : never = ['b', 'a']
