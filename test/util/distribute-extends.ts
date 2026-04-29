import type { Equal, DistributeExtends } from "../../src/util.ts"

type A = DistributeExtends<0, 0 | number> //=> true
const Expected_all_extends: true = {} as Equal<true, A> 

type B = DistributeExtends<0, 0> //=> true
const Expected_one_extends: true = {} as Equal<true, B>

type C = DistributeExtends<0, '0'> //=> false
const Expected_not_one_extends: true = {} as Equal<false, C>

type D = DistributeExtends<0, '0', false> //=> true
const Expected_not_one_extends1: true = {} as Equal<true, D>

type DD = DistributeExtends<0, '0' | {a: 0}, false> //=> true
const Expected_not_all_extends: true = {} as Equal<true, DD>

type E = DistributeExtends<0, '0' | 0, false> //=> false
const Expected_some_extends: true = {} as Equal<false, E>
