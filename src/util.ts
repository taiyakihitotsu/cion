// https://github.com/sindresorhus/type-fest/blob/main/source/is-equal.d.ts
export type Equal<
  A
, B> =
  [A, B] extends [B, A]
    ? _IsEqual<A, B>
  : false

type _IsEqual<
  A
, B> =
  (<G>() => G extends A & G | G ? 1 : 2) extends (<G>() => G extends B & G | G ? 1 : 2)
    ? true
  : false

// ---------------
// -- Extends
// ---------------

type Not<T extends boolean> =
  T extends true ? false : true 

type EqualExtends<T, U> = [T, U] extends [U, T] ? true : false

/**
```typescript
type A = DistributeExtends<0, 0 | number> //=> true
type B = DistributeExtends<0, 0> //=> true
type C = DistributeExtends<0, '0'> //=> false
type D = DistributeExtends<0, '0', false> //=> true
type DD = DistributeExtends<0, '0' | {a: 0}, false> //=> true
type E = DistributeExtends<0, '0' | 0, false> //=> false
```

NOTE: If the left compares with `A` unions `B`, but `A` and `B` themselves are Union Type,
wraps all arguments such as `DistributeExtends<[Source], [A] | [B]>`.
*/
export type DistributeExtends<T, U, Expect extends boolean = true> = 
  [ U extends infer u
      ? [ T extends u ? Expect : Not<Expect> ]
    : never ] extends infer Result
    ? EqualExtends<[[true]], Result>
  : never

// ---------------
// -- Record
// ---------------

export type DissocKeys<
  R extends Record<PropertyKey, unknown>
, Ks extends (keyof R)[]> =
Omit<R, Ks[number]>

export type AssocWith<
  R extends Record<PropertyKey, unknown>
, RR extends Record<PropertyKey, unknown>> =
{ [K in keyof R]: Equal<(keyof RR) & K, never> extends true ? R[K] : RR[K]}

export type UnionToIntersection<
  U> =
  (U extends any ? (k: U) => void : never) extends ((k: infer I) => void)
    ? I
  : never

export type LastOf<
  T> =
  [T] extends [never]
    ? never
  : UnionToIntersection<T extends any ? () => T : never> extends () => (infer R)
    ? R
  : never

export type UnionToTuple<
  U
, T extends any[] = []> =
  [U] extends [never]
    ? T
  : UnionToTuple<Exclude<U, LastOf<U>>, [LastOf<U>, ...T]>

export type KeysTuple<R extends Record<PropertyKey, unknown>> = UnionToTuple<keyof R>

// -------------------------
// -- about 2589 error
// -------------------------

export type _rec<
  T> =
  T extends {r: never}
    ? never
  : T extends {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: infer U}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
    ? { r: _rec<U> }
  : T extends {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: {r: infer U}}}}}}}}}}}}}}}}
    ? { r: _rec<U> }
  : T extends {r: {r: {r: {r: {r: {r: {r: {r: infer U}}}}}}}}
    ? { r: _rec<U> }
  : T extends {r: {r: {r: {r: infer U}}}}
    ? { r: _rec<U> }
  : T extends {r: {r: infer U}}
    ? { r: _rec<U> }
  : T extends {r: infer U}
    ? U
  : T

export type Rec<
  T> =
  T extends {r: unknown}
    ? Rec<_rec<T>>
  : T

export type * as Util from './util.js'
