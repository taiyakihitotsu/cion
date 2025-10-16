// https://github.com/sindresorhus/type-fest/commit/785549f36465e3f3d99a08832784b603261f74f2
export type Equal<
  A
, B> =
  [A, B] extends [infer AA, infer BB]
    ? [AA] extends [never]
      ? [BB] extends [never]
        ? true
      : false
    : [BB] extends [never]
      ? false
    : _IsEqual<AA, BB>
  : false

type _IsEqual<
  A
, B> =
  (<G>() => G extends A & G | G ? 1 : 2) extends (<G>() => G extends B & G | G ? 1 : 2)
    ? true
  : false


// ---------------
// -- Record
// ---------------

export type DissocKeys<
  R extends Record<PropertyKey, unknown>
, Ks extends (keyof R)[]> =
  Ks extends []
    ? R
  : Ks extends [infer fK extends keyof R, ...infer rK]
    ? rK extends Exclude<keyof R, fK>[]
      ? DissocKeys<Omit<R, fK>, rK>
    : never
  : never

// [todo]
// move to foxp
export type UnionToIntersection<
  U> =
  (U extends any ? (k: U) => void : never) extends ((k: infer I) => void)
    ? I
  : never
// [todo]
export type UtoI<U> = UnionToIntersection<U>
// [todo]
export type LastOf<
  T> =
  UnionToIntersection<T extends any ? () => T : never> extends () => (infer R)
    ? R
  : never
// [todo]
export type UnionToTuple<
  U
, T extends any[] = []> =
  [U] extends [never]
    ? T
  : UnionToTuple<Exclude<U, LastOf<U>>, [LastOf<U>, ...T]>
// [todo]
export type UtoT<U> = UnionToTuple<U>

export type KeysTuple<R extends Record<PropertyKey, unknown>> = UtoT<keyof R>

export type AssocWith<
  R extends Record<PropertyKey, unknown>
, RR extends Record<PropertyKey, unknown>> =
{ [K in keyof R]: RR[K] extends never ? R[K] : RR[K]}

// -------------------------
// -- about 2589 error
// -------------------------

type _rec<
  T> =
  T extends {r: never}
    ? never
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

export type * as Util from './util'
