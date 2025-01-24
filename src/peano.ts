// This file is used only to integrate a number of digits of bits.
// Not directly used as a number on type.
//
// This is picked from https://github.com/taiyakihitotsu/SaferArray
// but I think that should be rewritten with bit-number as well.
// ... 2024.05.19

import type Util from './util'

namespace Peano {
  export type T0 = null;
  export type T1 = [T0];
  export type T8 = [[[[[[[[Peano.T0]]]]]]]];
  export type T16 = Peano.mul<T8, [[null]]>;
  export type T32 = Peano.mul<T16, [[null]]>;
  export type T64 = Peano.mul<T32, [[null]]>;

  export const P0:T0 = null
  export const P1:T1 = [P0]
  export const P8:T8   = [[[[[[[[null]]]]]]]]
  export const P16:T16 = [[[[[[[[P8]]]]]]]]
  export const P32:T32 = [[[[[[[[[[[[[[[[P16]]]]]]]]]]]]]]]]
  export const P64:T64 = [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[P32]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

  export type add<T, TT> = TT extends T0
    ? T
    : T extends T0
      ? TT
      : TT extends [infer U]
        ? add<[T], U>
        : never;

  export type inc<T> = [T];

  export type dec<T> = T extends [infer U] ? U : never;

  export type min<T, TT> = TT extends T0
    ? T
    : TT extends [infer U]
      ? min<dec<T>, U>
      : never;

  export type mul<T, TT, I = T> = T extends T1
    ? TT
    : TT extends T1
      ? T
      : T extends T0
        ? T0
        : TT extends T0
          ? T0
          : TT extends [infer U]
            ? mul<add<T, I>, U, I>
            : never;

  export type TELesserUnion<T> = T extends [infer U]
    ? T | TELesserUnion<U>
    : never;
  export type TLesserUnion<T> = T extends [infer U] ? T0 | TELesserUnion<U> : never;

  export type gthan<T, U> = U extends TLesserUnion<T> ? true : false;
  export type gethan<T,U>   = Util.Equal<T,U> extends true ? true : gethan<T,U>
  export type lethan<T,U>  = gethan<U,T>
  export type lthan<T,U>   = gthan<U,T>
}

export default Peano;
