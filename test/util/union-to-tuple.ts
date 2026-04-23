import type { Equal, UnionToTuple as UtoT } from '../../src/util.js'

/**
`UnionToTuple`, `UtoT`
*/
const test_utot: true = {} as Equal<[1,2], UtoT<1 | 2> extends [(1|2), (1|2)] ? [1,2] : never>
// const test_ks: u.KeysTuple<{a: 1, b: 2}> extends [('b'|'a'), ('b'|'a')] ? ['b','a'] : never = ['b', 'a']
