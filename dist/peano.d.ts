import type Util from './util';
declare namespace Peano {
    type T0 = null;
    type T1 = [T0];
    type T8 = [[[[[[[[Peano.T0]]]]]]]];
    type T16 = Peano.mul<T8, [[null]]>;
    type T32 = Peano.mul<T16, [[null]]>;
    type T64 = Peano.mul<T32, [[null]]>;
    const P0: T0;
    const P1: T1;
    const P8: T8;
    const P16: T16;
    const P32: T32;
    const P64: T64;
    type add<T, TT> = TT extends T0 ? T : T extends T0 ? TT : TT extends [infer U] ? add<[T], U> : never;
    type inc<T> = [T];
    type dec<T> = T extends [infer U] ? U : never;
    type min<T, TT> = TT extends T0 ? T : TT extends [infer U] ? min<dec<T>, U> : never;
    type mul<T, TT, I = T> = T extends T1 ? TT : TT extends T1 ? T : T extends T0 ? T0 : TT extends T0 ? T0 : TT extends [infer U] ? mul<add<T, I>, U, I> : never;
    type TELesserUnion<T> = T extends [infer U] ? T | TELesserUnion<U> : never;
    type TLesserUnion<T> = T extends [infer U] ? T0 | TELesserUnion<U> : never;
    type gthan<T, U> = U extends TLesserUnion<T> ? true : false;
    type gethan<T, U> = Util.Equal<T, U> extends true ? true : lthan<U, T>;
    type lethan<T, U> = gethan<U, T>;
    type lthan<T, U> = gthan<U, T>;
}
export default Peano;
