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


// ------------------
// -- numeric
// ------------------

type StrToNum<T extends string> = T extends `${infer N extends number}` ? N : never;

// (x) 2n, 2n -> n, n
// (y) 2n, 2n+1 -> 2n, 2n -> n, n
// (z) 2n+1, 2n -> 2n-1, (f: 0->5, 2->6, 4->7, 6->8, 8->9)
// (zz) 2n+1, 2n+1 -> 2n+1, 2n -> (z)
type HalveCipher_Up  = {0: 5, 1: 5, 2: 6, 3: 6, 4: 7, 5: 7, 6: 8, 7: 8, 8: 9, 9: 9}
type HalveCipher_Div = {0: 0, 1: 0, 2: 1, 3: 1, 4: 2, 5: 2, 6: 3, 7: 3, 8: 4, 9: 4}
type HalveCipher_Dec = {0: 0, 1: 0, 2: 2, 3: 2, 4: 4, 5: 4, 6: 6, 7: 6, 8: 8, 9: 8}
type Digit = keyof HalveCipher_Div | keyof HalveCipher_Up
type Even = 0 | 2 | 4 | 6 | 8
type Odd  = 1 | 3 | 5 | 7 | 9
type Nat =
{ 0: never
    , 1: 0
    , 2: 1|0
    , 3: 2|1|0
    , 4: 3|2|1|0
    , 5: 4|3|2|1|0
    , 6: 5|4|3|2|1|0
    , 7: 6|5|4|3|2|1|0
    , 8: 7|6|5|4|3|2|1|0
    , 9: 8|7|6|5|4|3|2|1|0 }

// This is used for a culc of pivot.
const Test_LooseHalve_0: LooseHalve< 0> = 0
const Test_LooseHalve_1: LooseHalve< 2> = 1
const Test_LooseHalve_2: LooseHalve<59> = 29
const Test_LooseHalve_3: LooseHalve<60> = 30
const Test_LooseHalve_4: LooseHalve<61> = 30
const Test_LooseHalve_5: LooseHalve<71> = 35
type LooseHalve<
  N extends number> =
  `${N}` extends `${infer F extends number}${infer SS}`
    ? '' extends SS
      ? HalveCipher_Div[F extends Digit ? F : never]
    : SS extends `${infer S extends number}${infer T}`
      ? '' extends T
        ? [F, S] extends infer Nums extends [Digit, Digit]
          ? Nums[0] extends Even
            ? StrToNum<`${HalveCipher_Div[Nums[0]]}${HalveCipher_Div[Nums[1]]}`>
          : StrToNum<`${HalveCipher_Div[Nums[0]]}${HalveCipher_Up[HalveCipher_Dec[Nums[1]]]}`>
        : never
      : never
    : never
  : never

const TestLessThan_0: LessThan<9,9> = false
const TestLessThan_1: LessThan<9,8> = false
const TestLessThan_2: LessThan<8,9> = true
const TestLessThan_3: LessThan<15,15> = false
const TestLessThan_4: LessThan<15,14> = false
const TestLessThan_6: LessThan<14,15> = true
const TestLessThan_7: LessThan<25,14> = false
const TestLessThan_8: LessThan<14,25> = true

type LessThan<
  Left extends number
, Right extends number> =
  Left extends Left & Right
    ? false
  : `${Left}/${Right}` extends `${infer LF extends Digit}${infer LS extends Digit}/${infer RF extends Digit}${infer RS extends Digit}`
    ? LF extends Nat[RF]
      ? true
    : [LF, LS] extends [LF & RF, Nat[RS]]
      ? true
    : false
  : Left extends Nat[Right extends Digit ? Right : never]
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
