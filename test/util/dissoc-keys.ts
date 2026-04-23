import type { DissocKeys, AssocWith, Equal, UnionToTuple as UtoT } from '../../src/util.js'

/**
`DissocKeys`
*/
const test_dissoc0: true = {} as Equal<DissocKeys<{a: 1, b: 2}, ['b']>, {a: 1}>
const test_dissoc1: false = {} as Equal<DissocKeys<{a: 1, b: 2}
  // @ts-expect-error:
  , ['c']>
  , {a: 1}>
const test_dissoc2: false = {} as Equal<DissocKeys<{a: 1, b: 2}, ['b']>, {b: 1}>
const test_dissoc3: true = {} as Equal<DissocKeys<{a: 1, b: 2}, ['b', 'a']>, {}>
// @ts-expect-error:
const test_dissoc4: false = {} as Equal<DissocKeys<{}
  // @ts-expect-error:
  , ['b', 'a']>
  , {}>
