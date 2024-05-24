// This file is used only to integrate a number of digits of bits.
// Not directly used as a number on type.
//
// This is picked from https://github.com/taiyakihitotsu/SaferArray
// but I think that should be rewritten with bit-number as well.
// ... 2024.05.19

namespace Peano {
  export type T0 = null;

  export type T1 = [T0];

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
  export type TLesserUnion<T> = T extends [infer U] ? TELesserUnion<U> : never;

  export type gthan<T, U> = U extends TLesserUnion<T> ? true : false;
}

export default Peano;
