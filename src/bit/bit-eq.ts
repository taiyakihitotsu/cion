/**
Return `true` if both bit AND bit-length are equal.

This does not uniform the difference of bit-length, because of performance.
*/
export type BitEq<
  B extends string
, C extends string> =
  B extends ""
    ? C extends ""
      ? true
    : false
  : B extends `${infer HB extends '0' | '1'}${infer TB}`
    ? C extends `${infer HC extends '0' | '1'}${infer TC}`
      ? HC extends HB
        ? HB extends HC
          ? BitEq<TB, TC>
        : false
      : false
    : false
  : false
