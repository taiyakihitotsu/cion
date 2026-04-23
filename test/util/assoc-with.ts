import type { DissocKeys, AssocWith, Equal, UnionToTuple as UtoT } from '../../src/util.js'

/**
`AssocWith`
*/
type assocwith0 = AssocWith<{a: 0, b: 1}, {a: string, b: 9}>
const test_assocwith_0: true = {} as Equal<{a: string, b: 9}, assocwith0>
type assocwith1 = AssocWith<{a: string, b: 9}, {a: 0, b: 1}>
const test_assocwith_1: true = {} as Equal<{a: 0, b: 1}, assocwith1>
type assocwith2 = AssocWith<{a: 0, b: 1}, {a: string}>
const test_assocwith_2: true = {} as Equal<{a: string, b: 1}, assocwith2>
type assocwith3 = AssocWith<{a: string}, {a: 0, b: 1}>
const test_assocwith_3: true = {} as Equal<{a: 0}, assocwith3>
type assocwith4 = AssocWith<{a: string, b: 9}, {c: 0}>
const test_assocwith_4: true = {} as Equal<{a: string, b: 9}, assocwith4>
type assocwith5 = AssocWith<{c: 0}, {a: string, b: 9}>
const test_assocwith_5: true = {} as Equal<{c: 0}, assocwith5>
